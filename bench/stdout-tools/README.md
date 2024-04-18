# Benchs vs jq/grep

nix-shell OR nix-shell -p ghc cabal-install haskellPackages.eventlog2html jq

## Benchmarks using a log file that is five nodes' ```stdout``` files together

```
ls -lah bench/stdout-tools/5nodes.stdout
-rw-r--r-- 1 dev users 6.4G Apr 10 19:28 bench/stdout-tools/5nodes.stdout
```

### Count all lines

Here we can observe the overhead of reading every line as a JSON trace message.

#### ```wc```:
```
time cat bench/stdout-tools/5nodes.stdout | wc -l
25581640
real    0m2.039s
user    0m0.091s
sys     0m2.842s
```
#### ```jq```:
```
time jq --raw-input . bench/stdout-tools/5nodes.stdout | wc -l
25581640
real    1m32.285s
user    1m29.735s
sys     0m8.123s
```
#### ```tq```:
```
time cabal run tq -- --file big-node:bench/stdout-tools/5nodes.stdout --reducer count-lines
25581640
real    0m15.385s
user    0m13.138s
sys     0m2.239s
```

### Count all the ```ns="Forge.Loop.StartLeadershipCheckPlus"```

Also the overhead of being able to query JSON trace messages.

#### ```grep```
```
time grep -E "^{.*" bench/stdout-tools/5nodes.stdout | grep "Forge.Loop.StartLeadershipCheckPlus" | wc -l
264150
real    0m4.250s
user    0m3.525s
sys     0m3.694s
```
#### Using ```jq``` for everything:
```
time jq --raw-input --compact-output 'try fromjson | if (type == "object" and has("at")) then select(.ns=="Forge.Loop.StartLeadershipCheckPlus") else empty end' bench/stdout-tools/5nodes.stdout | wc -l
264150
real    1m30.825s
user    1m29.308s
sys     0m1.567s
```
#### Using ```jq``` but first filtering non valid JSON lines with ```grep```:
```
time grep -E "^{.*" bench/stdout-tools/5nodes.stdout | jq --compact-output 'select(.ns == "Forge.Loop.StartLeadershipCheckPlus")' | wc -l
264150
real    1m9.565s
user    1m11.935s
sys     0m6.041s
```
#### ```tq```:
```
time cabal run tq -- --file big-node:bench/stdout-tools/5nodes.stdout --reducer count-FLSLCP
264150
real    0m19.904s
user    0m17.629s
sys     0m2.259s
```

### Heap changes

#### Using ```jq``` but first filtering non valid JSON lines with ```grep```:
```
time grep -E "^{.*" bench/stdout-tools/5nodes.stdout | jq 'select(.ns == "Resources") | .data.Heap' | uniq
real    1m8.410s
user    1m10.831s
sys     0m5.905s
```
#### ```tq```:
```
time cabal run tq -- --file big-node:bench/stdout-tools/5nodes.stdout --reducer heap-changes
real    0m21.704s
user    0m19.414s
sys     0m2.280s
```

## Benchmarks using an entire 52 nodes run

```
$ ls -lah run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-*/stdout
-rw-r--r-- 1 dev users  1.1G Apr  9 16:36 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-0/stdout
-rw-r--r-- 1 dev users  1.1G Apr  9 16:36 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-10/stdout
-rw-r--r-- 1 dev users  1.1G Apr  9 16:36 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-11/stdout
-rw-r--r-- 1 dev users  1.1G Apr  9 16:37 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-12/stdout
-rw-r--r-- 1 dev users  1.1G Apr  9 16:37 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-13/stdout
-rw-r--r-- 1 dev users 1017M Apr  9 16:37 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-14/stdout
-rw-r--r-- 1 dev users  1.1G Apr  9 16:37 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-15/stdout
-rw-r--r-- 1 dev users  1.1G Apr  9 16:37 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-16/stdout
-rw-r--r-- 1 dev users  997M Apr  9 16:37 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-17/stdout
-rw-r--r-- 1 dev users  1.1G Apr  9 16:37 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-18/stdout
-rw-r--r-- 1 dev users  1.1G Apr  9 16:37 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-19/stdout
-rw-r--r-- 1 dev users  1.2G Apr  9 16:36 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-1/stdout
-rw-r--r-- 1 dev users 1012M Apr  9 16:37 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-20/stdout
-rw-r--r-- 1 dev users  1.1G Apr  9 16:38 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-21/stdout
-rw-r--r-- 1 dev users  1.1G Apr  9 16:38 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-22/stdout
-rw-r--r-- 1 dev users 1019M Apr  9 16:38 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-23/stdout
-rw-r--r-- 1 dev users  1.1G Apr  9 16:38 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-24/stdout
-rw-r--r-- 1 dev users  1.1G Apr  9 16:38 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-25/stdout
-rw-r--r-- 1 dev users  1.1G Apr  9 16:38 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-26/stdout
-rw-r--r-- 1 dev users  1.1G Apr  9 16:38 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-27/stdout
-rw-r--r-- 1 dev users  1.1G Apr  9 16:38 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-28/stdout
-rw-r--r-- 1 dev users  1.1G Apr  9 16:38 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-29/stdout
-rw-r--r-- 1 dev users  1.1G Apr  9 16:37 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-2/stdout
-rw-r--r-- 1 dev users  1.1G Apr  9 16:39 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-30/stdout
-rw-r--r-- 1 dev users  1.2G Apr  9 16:39 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-31/stdout
-rw-r--r-- 1 dev users  1.1G Apr  9 16:39 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-32/stdout
-rw-r--r-- 1 dev users  1.1G Apr  9 16:39 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-33/stdout
-rw-r--r-- 1 dev users  1.1G Apr  9 16:39 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-34/stdout
-rw-r--r-- 1 dev users  976M Apr  9 16:40 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-35/stdout
-rw-r--r-- 1 dev users  1.1G Apr  9 16:40 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-36/stdout
-rw-r--r-- 1 dev users  1.1G Apr  9 16:40 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-37/stdout
-rw-r--r-- 1 dev users 1013M Apr  9 16:40 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-38/stdout
-rw-r--r-- 1 dev users  1.1G Apr  9 16:40 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-39/stdout
-rw-r--r-- 1 dev users  1.1G Apr  9 16:38 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-3/stdout
-rw-r--r-- 1 dev users  1.1G Apr  9 16:40 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-40/stdout
-rw-r--r-- 1 dev users 1008M Apr  9 16:40 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-41/stdout
-rw-r--r-- 1 dev users  1.1G Apr  9 16:40 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-42/stdout
-rw-r--r-- 1 dev users  1.1G Apr  9 16:41 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-43/stdout
-rw-r--r-- 1 dev users 1009M Apr  9 16:41 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-44/stdout
-rw-r--r-- 1 dev users  1.1G Apr  9 16:41 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-45/stdout
-rw-r--r-- 1 dev users  1.1G Apr  9 16:41 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-46/stdout
-rw-r--r-- 1 dev users  995M Apr  9 16:41 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-47/stdout
-rw-r--r-- 1 dev users  1.1G Apr  9 16:41 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-48/stdout
-rw-r--r-- 1 dev users  1.1G Apr  9 16:42 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-49/stdout
-rw-r--r-- 1 dev users  1.1G Apr  9 16:40 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-4/stdout
-rw-r--r-- 1 dev users 1001M Apr  9 16:42 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-50/stdout
-rw-r--r-- 1 dev users  976M Apr  9 16:42 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-51/stdout
-rw-r--r-- 1 dev users 1014M Apr  9 16:42 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-5/stdout
-rw-r--r-- 1 dev users  1.1G Apr  9 16:42 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-6/stdout
-rw-r--r-- 1 dev users  1.1G Apr  9 16:42 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-7/stdout
-rw-r--r-- 1 dev users 1009M Apr  9 16:42 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-8/stdout
-rw-r--r-- 1 dev users  1.1G Apr  9 16:42 run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-9/stdout
```

REDO:
### Heap changes (sequential)

#### Using ```jq``` but first filtering non valid JSON lines with ```grep```:
```
time for i in `seq 0 51`; do echo "node-$i" && grep -E "^{.*" run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-"$i"/stdout | jq --compact-output 'if .ns == "Resources" then .data.Heap else empty end' | uniq &; done
real    9m40.413s
user    9m49.158s
sys     1m4.572s
```
#### ```tq```:
```
cabal run tq -- --run run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom --reducer heap-changes
real    9m10.366s
user    8m12.345s
sys     0m46.550s
```

### Heap changes (in parallel)

16 cores machine

#### Using ```jq``` but first filtering non valid JSON lines with ```grep```:
```
time bash -c "array=(); for i in \`seq 0 51\`; do echo \"node-\$i\" && grep -E \"^{.*\" run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-\"\$i\"/stdout | jq --compact-output 'if .ns == \"Resources\" then .data.Heap else empty end' | uniq & array+=(\"\$!\"); done; wait \"\${array[@]}\""
real    2m55.609s
user    43m52.673s
sys     2m25.412s
```
#### ```tq```:
```
time cabal run tq -- --run run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom --parallel --reducer heap-changes +RTS -N16
real    2m33.630s
user    38m24.001s
sys     1m3.555s
```

32 cores machine

#### Using ```jq``` but first filtering non valid JSON lines with ```grep```:
```
time bash -c "array=(); for i in \`seq 0 51\`; do echo \"node-\$i\" && grep -E \"^{.*\" run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-\"\$i\"/stdout | jq --compact-output 'if .ns == \"Resources\" then .data.Heap else empty end' | uniq & array+=(\"\$!\"); done; wait \"\${array[@]}\""
real    0m49.256s
user    21m49.630s
sys     2m37.604s
```
#### ```tq```:
```
time cabal run tq -- --run run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom --parallel --reducer heap-changes +RTS -N16
real    1m8.057s
user    31m6.545s
sys     2m26.358s
```
