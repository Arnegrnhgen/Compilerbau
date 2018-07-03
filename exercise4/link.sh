#!/bin/sh

set -eu

DEST=$(stack exec which lab4)
ln -f -s "$DEST" lab4
