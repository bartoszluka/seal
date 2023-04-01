#!/bin/env fish
mkdir -p out
set filename (string split --right --max 1 --fields 2 / $argv[1]); or set filename $argv[1]
and set name_no_ext (string split --right --max 1 --fields 1 . $filename)
and stack run compile $argv[1] >out/"$name_no_ext".ll
and llc -filetype=asm out/"$name_no_ext".ll -o out/"$name_no_ext".s
and gcc -c -no-pie out/"$name_no_ext".s -o out/"$name_no_ext".o
and gcc -no-pie out/"$name_no_ext".o -o out/"$name_no_ext"
and echo (set_color green)successfully compiled!
or echo (set_color red)"there was an error :("
