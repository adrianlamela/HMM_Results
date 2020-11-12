#!/bin/bash
for i in $(ls *.outA); do
        sort $i > $i.soutA
done
rename 's/\.outA\.soutA/\.soutA/' *
