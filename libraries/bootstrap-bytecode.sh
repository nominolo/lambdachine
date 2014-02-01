#!/bin/bash
# TODO: Replace this script by a Perl script (or Python or Ruby)
if [ $# -ne 3 ]; then
    echo "Please use 'make bytecode' to run this script with the right arguments"
    exit 1
fi
LCC=$1
HC_PKG=$2
PKG_DB=$3
DIR=`pwd`

YELLOW='\033[0;33m'
NC='\033[0m' # No Color

# Remove any existing package DB
if [ -d "${PKG_DB}" ]; then
    if [ -f "${PKG_DB}/package.cache" ]; then
        rm -r ${PKG_DB}
    else
        echo "ERROR: ${PKG_DB} does not look like a package database."
        echo "For safety reasons I will therefore not delete it."
        exit 1
    fi
fi

${HC_PKG} init ${PKG_DB}

sed 's:\${DIR}:'"${DIR}"'/rts:g' <rts/recipe.in >rts/recipe
${HC_PKG} register --global-package-db=${PKG_DB} rts/recipe

MODULES_ghc_prim="GHC/Types.hs GHC/Tuple.hs GHC/Magic.hs GHC/Classes.hs
                  GHC/CString.hs"
cd ghc-prim
for module in ${MODULES_ghc_prim}; do
  if [ "${module%.hs*}.hi" -ot "${module}" -a "${module%.hs*}.hi" -ot "${LCC}" ]
  then
      echo "Skipping ${module}"
      continue
  fi
  echo "Compiling ${YELLOW}ghc-prim:${module}${NC}";
  ../${LCC} -o2 --package-name=ghc-prim ${module} || exit 1
done
touch libHSghc-prim-0.3.0.0.a
cd ..
echo "Registering ${YELLOW}ghc-prim${NC}"
sed 's:\${DIR}:'"${DIR}"'/ghc-prim:g' <ghc-prim/recipe.in >ghc-prim/recipe
${HC_PKG} register --global-package-db=${PKG_DB} ghc-prim/recipe --force

MODULES_integer_simple="GHC/Integer/Type.hs GHC/Integer/Simple/Internals.hs GHC/Integer.hs"
cd integer-simple

for module in ${MODULES_integer_simple}; do
  if [ "${module%.hs*}.hi" -ot "${module}" -a "${module%.hs*}.hi" -ot "${LCC}" ]
  then
      echo "Skipping ${module}"
      continue
  fi
  echo "Compiling ${YELLOW}integer-simple:${module}${NC}"
  ../${LCC} -o2 --package-name=integer-simple ${module} || exit 1
done
touch libHSinteger-simple-0.1.0.1.a
cd ..
echo "Registering ${YELLOW}integer-simple${NC}"
sed 's:\${DIR}:'"${DIR}"'/integer-simple:g' <integer-simple/recipe.in >integer-simple/recipe
${HC_PKG} register --global-package-db=${PKG_DB} --force integer-simple/recipe

MODULES_base="GHC/Err.hs-boot Control/Exception/Base.hs GHC/Base.hs
  Data/Maybe.hs GHC/List.hs GHC/Show.hs GHC/Char.hs GHC/Enum.hs GHC/Num.hs
  GHC/Real.hs
  GHC/Err.hs"

# " GHC/Classes.hs GHC/Show.hs-boot GHC/Err.hs-boot GHC/Base.hs Data/Tuple.hs GHC/Enum.hs Data/Maybe.hs GHC/List.hs GHC/Show.hs GHC/Num.hs GHC/Err.hs GHC/Real.hs GHC/Unicode.hs-boot GHC/Unicode.hs Data/Char.hs Data/Either.hs Data/List.hs Data/Monoid.hs Control/Monad.hs Text/ParserCombinators/ReadP.hs Text/ParserCombinators/ReadPrec.hs Text/Read/Lex.hs GHC/Read.hs Text/Read.hs"
cd base
for module in ${MODULES_base}; do
  echo "Compiling ${YELLOW}base:${module}${NC}"
  ../${LCC} -o2 --package-name=base ${module} || exit 1
done
touch libHSbase-4.6.0.1.a
cd ..
echo "Registering ${YELLOW}base${NC}"
sed 's:\${DIR}:'"${DIR}"'/base:g' <base/recipe.in >base/recipe
${HC_PKG} register --global-package-db=${PKG_DB} --force base/recipe

exit 42


MODULES_containers="Data/Set.hs Data/Map.hs"
cd containers
for module in ${MODULES_containers}; do
  echo "Compiling ${YELLOW}containers:${module}${NC}"
  ../${LCC} -o2 --package-name=containers-0.4.0.0 ${module} || exit 1
done
touch libHScontainers-0.4.0.0.a
cd ..
echo "Registering ${YELLOW}containers${NC}"
sed 's:\${DIR}:'"${DIR}"'/containers:g' <containers/recipe.in >containers/recipe
${HC_PKG} register --global-package-db=${PKG_DB} --force containers/recipe

exit 0
