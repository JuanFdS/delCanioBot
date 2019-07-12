BUILD_PATH=build/

if [ ! -d $BUILD_PATH ]; then
    mkdir -p $BUILD_PATH
fi

cp `stack exec which delCanio-exe` build/
