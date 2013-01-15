#!/bin/sh
if [ $# -ne 3 ]; then
    echo "Please use 'make bytecode' to run this script with the right arguments"
    exit 1
fi
LCC=$1
HC_PKG=$2
PKG_DB=$3

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

${HC_PKG} register --global-conf=${PKG_DB} rts/recipe

MODULES_ghc_prim="GHC/Bool.hs GHC/Ordering.hs GHC/Tuple.hs GHC/Unit.hs GHC/Types.hs"
cd ghc-prim
for module in ${MODULES_ghc_prim}; do
  echo "Compiling ${YELLOW}ghc-prim:${module}${NC}";
  ../${LCC} -o2 --package-name=ghc-prim ${module} || exit 1
done
cd ..
echo "Registering ${YELLOW}ghc-prim${NC}"
${HC_PKG} register --global-conf=${PKG_DB} ghc-prim/recipe --force


MODULES_integer_simple="GHC/Integer/Type.hs GHC/Integer/Simple/Internals.hs GHC/Integer.hs"
cd integer-simple

for module in ${MODULES_integer_simple}; do
  echo "Compiling ${YELLOW}integer-simple:${module}${NC}"
  ../${LCC} -o2 --package-name=integer-simple ${module} || exit 1
done
touch libHSinteger-simple-0.1.0.0.a
cd ..
echo "Registering ${YELLOW}integer-simple${NC}"
${HC_PKG} register --global-conf=${PKG_DB} --force integer-simple/recipe


MODULES_base="Control/Exception/Base.hs GHC/Classes.hs GHC/Show.hs-boot GHC/Err.hs-boot GHC/Base.hs Data/Tuple.hs GHC/Enum.hs Data/Maybe.hs GHC/List.hs GHC/Show.hs GHC/Num.hs GHC/Err.hs GHC/Real.hs GHC/Unicode.hs-boot GHC/Unicode.hs Data/Char.hs Data/Either.hs Data/List.hs Data/Monoid.hs"
cd base
for module in ${MODULES_base}; do
  echo "Compiling ${YELLOW}base:${module}${NC}"
  ../${LCC} -o2 --package-name=base ${module} || exit 1
done
touch libHSbase-4.3.1.0.a
cd ..
echo "Registering ${YELLOW}base${NC}"
${HC_PKG} register --global-conf=${PKG_DB} --force base/recipe

exit 0
