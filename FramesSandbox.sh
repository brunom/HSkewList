#!/bin/bash
set -e -v
#rm -rf FramesSandbox ; true
mkdir FramesSandbox
cd FramesSandbox
git clone https://github.com/acowley/Frames.git # includes most test data
cabal get vinyl
mv vinyl* vinyl
echo "packages:
 Frames/
 vinyl/
package Frames
 flags: +demos
 benchmarks: True
jobs: 4
" > cabal.project
cabal new-build Frames:bench:insurance
