#!/bin/sh

set -eu

DEST=$(stack exec which lab3)
ln -f -s "$DEST" lab3
