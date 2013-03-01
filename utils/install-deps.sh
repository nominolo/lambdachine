if [ $# -ne 3 ]; then
    echo "Please use 'make install-deps' to run this script with the right arguments"
    exit 1
fi
HC=$1
HC_PKG=$2
CABAL=$3

echo "HC = ${HC}, HC_PKG = ${HC_PKG}, CABAL = ${CABAL}"

${CABAL} install --with-compiler=${HC} --with-hc-pkg=${HC_PKG} cmdargs-0.6.2 --reinstall

