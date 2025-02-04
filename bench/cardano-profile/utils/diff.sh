#!/bin/bash

# Directories with all the profiles and its files to compare.
echo "- Directory 1: $1"
echo "- Directory 2: $2"

# TODO:
# backend="supervisor"

for sup_file_name in "$1"/*
do

  profile_name="$(basename "${sup_file_name}")"
  echo -e "\n\n\n"
  echo "---------- ---------- ---------- ----------"
  echo "---------- ${profile_name}:"
  echo "---------- ---------- ---------- ----------"
  echo -e "\n\n\n"

  # Profile files.
  echo "---------- ---------- profile:"
  for filepath in "$1"/"${profile_name}"/profile/*
  do
    filename="$(basename "${filepath}")"
    echo "---------- ---------- ---------- ${filename}:"
    if test "${filename}" = "profile.json"
    then
         dyff between \
           "$1/${profile_name}/profile/${filename}" \
           "$2/${profile_name}/profile/${filename}" \
      || read -r
    elif test "${filename}" = "generator-service.json"   \
      || test "${filename}" = "healthcheck-service.json" \
      || test "${filename}" = "tracer-service.json"
    then
      for key in $(jq -r 'keys | join (" ")' "$1/${profile_name}/profile/${filename}")
      do
        echo "---------- ---------- ---------- ---------- ${key}:"
        if test "${key}" = "name"
        then
          name1="$(jq -r ".name" "$1/${profile_name}/profile/${filename}")"
          name2="$(jq -r ".name" "$2/${profile_name}/profile/${filename}")"
          if ! test "$name1" = "$name2"
          then
            echo "${name1} /= ${name2}"
            exit 1
          fi
        else
             diff \
               "$(jq -r ".[\"${key}\"]" "$1/${profile_name}/profile/${filename}")" \
               "$(jq -r ".[\"${key}\"]" "$2/${profile_name}/profile/${filename}")" \
          || read -r
        fi
      done
    elif test "${filename}" = "node-services.json"
    then
      for index in $(jq -r 'keys | join (" ")' "$1/${profile_name}/profile/${filename}")
      do
        for key in $(jq -r ".[\"${index}\"] | keys | join (\" \")" "$1/${profile_name}/profile/${filename}")
        do
          echo "---------- ---------- ---------- ---------- .[${index}].${key}:"
          if test "${key}" = "name"
          then
            name1="$(jq -r ".[\"${index}\"].name" "$1/${profile_name}/profile/${filename}")"
            name2="$(jq -r ".[\"${index}\"].name" "$2/${profile_name}/profile/${filename}")"
            if ! test "$name1" = "$name2"
            then
              echo "${name1} /= ${name2}"
              exit 1
            fi
          else
               diff \
                 "$(jq -r ".[\"${index}\"].${key}" "$1/${profile_name}/profile/${filename}")" \
                 "$(jq -r ".[\"${index}\"].${key}" "$2/${profile_name}/profile/${filename}")" \
            || read -r
          fi
        done
      done
    elif test "${filename}" = "workloads-service.json"
    then
      for index in $(jq -r 'keys | join (" ")' "$1/${profile_name}/profile/${filename}")
      do
        for key in $(jq -r ".[${index}] | keys | join (\" \")" "$1/${profile_name}/profile/${filename}")
        do
          echo "---------- ---------- ---------- ---------- .[${index}].${key}:"
          if test "${key}" = "name"
          then
            name1="$(jq -r ".[${index}].name " "$1/${profile_name}/profile/${filename}")"
            name2="$(jq -r ".[${index}].name " "$2/${profile_name}/profile/${filename}")"
            if ! test "$name1" = "$name2"
            then
              echo "${name1} /= ${name2}"
              exit 1
            fi
          else
               diff \
                 "$(jq -r ".[${index}].${key} " "$1/${profile_name}/profile/${filename}")" \
                 "$(jq -r ".[${index}].${key} " "$2/${profile_name}/profile/${filename}")" \
            || read -r
          fi
        done
      done
    else
         diff \
           "$1/${profile_name}/profile/${filename}" \
           "$2/${profile_name}/profile/${filename}" \
      || read -r
    fi
  done

  file1="$(mktemp)"
  file2="$(mktemp)"
  # Backend files.
  echo "---------- ---------- Backend files:"
  for filepath in "$1"/"$profile_name"/backend/*
  do
    filename="$(basename "${filepath}")"
    if test "${filename}" = "container-specs.json"
    then
      jq '.nomadJob.cloud.nomadExec |= null | .nomadJob.cloud.ssh |= null' "$1/${profile_name}/backend/${filename}" > "${file1}"
      jq '.nomadJob.cloud.nomadExec |= null | .nomadJob.cloud.ssh |= null' "$2/${profile_name}/backend/${filename}" > "${file2}"
      dyff between "${file1}" "${file2}"
#      jq -r '.nomadJob.cloud.nomadExec' "$1/${profile_name}/backend/${filename}" > "${file1}"
#      jq -r '.nomadJob.cloud.nomadExec' "$2/${profile_name}/backend/${filename}" > "${file2}"
#      dyff between "${file1}" "${file2}"
#      jq -r '.nomadJob.cloud.ssh' "$1/${profile_name}/backend/${filename}" > "${file1}"
#      jq -r '.nomadJob.cloud.ssh' "$2/${profile_name}/backend/${filename}" > "${file2}"
#      dyff between "${file1}" "${file2}"
    else
      echo "---------- ---------- ---------- ${filename}"
         diff \
           "$1/${profile_name}/backend/${filename}" \
           "$2/${profile_name}/backend/${filename}" \
      || read -r
    fi
  done

done

