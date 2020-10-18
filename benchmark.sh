#!/bin/sh

# assumes fast-tldr is installed.

mkdir -p benchmarks && cd benchmarks

TESTPAGE=tar

# get tealdeer
if [ ! -f tealdeer ]; then
  wget https://github.com/dbrgn/tealdeer/releases/download/v1.4.1/tldr-linux-x86_64-musl
  mv tldr-linux-x86_64-musl tealdeer
  chmod +x tealdeer
  ./tealdeer --update
fi

if [ ! -f tldr-c-client ]; then
  wget https://github.com/tldr-pages/tldr-c-client/archive/v1.3.0.zip
  unzip v1.3.0.zip
  cd tldr-c-client-1.3.0
  make
  mv ./tldr ../tldr-c-client
  cd ..
  ./tldr-c-client --update
fi

if [ ! -f tldr-bash-client ]; then
  wget -qO tldr-bash-client https://4e4.win/tldr
  sudo chmod +x tldr-bash-client
  ./tldr-bash-client update
fi

if [ ! -f tldr-go-client ]; then
  wget https://bitbucket.org/djr2/tldr/raw/f412bddb2a583ddd8192587bb33d0260bc4945d0/dist/linux.tar.bz2
  tar xvjf linux.tar.bz2
  mv dist/build/linux/tldr tldr-go-client
  chmod +x tldr-go-client
  ./tldr-go-client --update
fi


hyperfine --prepare 'sync; echo 3 | sudo tee /proc/sys/vm/drop_caches' \
          "tldr $TESTPAGE" \
          "./tealdeer $TESTPAGE" \
          "./tldr-c-client $TESTPAGE" \
          "./tldr-bash-client $TESTPAGE" \
          "./tldr-go-client $TESTPAGE"