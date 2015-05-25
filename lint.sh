#!/bin/bash
set -e

# Lint all modules
find . -type f -iname "*.pm" -print0 | while IFS= read -r -d $'\0' file; do
	perl -Mstrict -Mdiagnostics -cw $file
done

# perlcritic --harsh all modules	
find . -type f -iname "*.pm" -print0 | while IFS= read -r -d $'\0' file; do
	perlcritic --harsh $file
done
