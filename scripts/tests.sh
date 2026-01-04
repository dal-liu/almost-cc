#!/usr/bin/env bash

function generateTests {

  # Fetch the inputs
  srcLang=$1 ;
  echo "Considering $srcLang" ;

  # Go the to compiler directory
  pushd ./ &> /dev/null ;
  cd $srcLang ;

  # Generate the oracle of the new tests
  make test ;
  echo "" ;

  # Leave
  popd &> /dev/null ;
  return ;
}

generateTests "ir" ;
generateTests "l3" ;
generateTests "l2" ;
generateTests "l1" ;
