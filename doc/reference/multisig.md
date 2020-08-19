# Multi-Signature Scripts

A multi-signature scheme allows an unspent transaction output to be used as an input to
a new transaction if a pre-defined combination of signatures is provided, e.g., two persons
have to sign simultaneously, two out of three keys have to be provided, etc.

The multi-signature scripts are written as JSON objects. We have three types of scripts:

## all

The "all" key indicates that in order to spend this tx output, we require the corresponding signatures of all the payment key hashes listed.

```json

{
    "all": [
        "e09d36c79dec9bd1b3d9e152247701cd0bb860b5ebfd1de8abb6735a",
        "a687dcc24e00dd3caafbeb5e68f97ca8ef269cb6fe971345eb951756",
        "0bd1d702b2e6188fe0857a6dc7ffb0675229bab58c86638ffa87ed6d",
        "dd0044a26cf7d4491ecea720fda11afb59d5725b53afa605fdf695e6",
        "cf223afe150cc8e89f11edaacbbd55b011ba44fbedef66fbd37d8c9d",
        "372643e7ef4b41fd2649ada30a89d35cb90b7c14cb5de252e6ce6cb7",
        "aa453dc184c5037d60e3fbbadb023f4a41bac112f249b76be9bb37ad",
        "6b732c60c267bab894854d6dd57a04a94e603fcc4c36274c9ed75952"
    ]
}
```



## any

The "any" key indicates that in order to spend this tx output, we require one corresponding signature from the listed payment key hashes.

```json
{
    "any": [
        "d92b712d1882c3b0f75b6f677e0b2cbef4fbc8b8121bb9dde324ff09",
        "4d780ed1bfc88cbd4da3f48de91fe728c3530d662564bf5a284b5321",
        "3a94d6d4e786a3f5d439939cafc0536f6abc324fb8404084d6034bf8",
        "b12e094d1db7c0fba5121f22db193d0060efed8be43654f861bb68ae",
        "9be49d56442b4b8b16cab4e43e238bbdefc6c803d554c82fcd5facc3",
        "622be5fab3b5c3f371a50a535e4d3349c942a98cecee93b24e2fd11d"
    ]
}
```

## atLeast

The "atLeast" key indicates that in order to spend this tx output, we require "atLeast" 2 corresponding signatures from the list of payment key hashes. The "required" key indicates the minimum number of signatures we need to spend the tx output.

```json
{
    "atLeast": {
        "list": [
            "2f3d4cf10d0471a1db9f2d2907de867968c27bca6272f062cd1c2413",
            "f856c0c5839bab22673747d53f1ae9eed84afafb085f086e8e988614",
            "b275b08c999097247f7c17e77007c7010cd19f20cc086ad99d398538",
            "686024aecb5884d73a11b9ae4e63931112ba737e878d74638b78513a"
        ],
        "required": 2
    }
}
```
