### Installing EGGX/ProCALL

Currently, FC8 depends on the [EGGX/ProCALL](https://www.ir.isas.jaxa.jp/~cyamauch/eggx_procall/) library. This can be installed via the following steps:

```txt
$ wget https://www.ir.isas.jaxa.jp/~cyamauch/eggx_procall/eggx-0.95.tar.gz
$ # or, alternatively
$ curl -o eggx-0.95.tar.gz https://www.ir.isas.jaxa.jp/~cyamauch/eggx_procall/eggx-0.95.tar.gz
$ tar -xf eggx-0.95.tar.gz && cd eggx-0.95
$ make
$ make install
```
(By default, the EGGX Makefile will try to install the library to `/usr/local/lib`, where super user rights might be needed.)

To install EGGX to a custom directory use:
```
$ make install PREFIX=<custom directory>
```
If EGGX was installed at a custom location, you should communicate this to CMake at configuration time:
```
$ cmake .. -DEGGX_DIR=<custom directory>
```

Note that EGGX/ProCALL requires a working X11/Xlib library. On Ubuntu you can install it using `sudo apt-get install libx11-dev`. On Mac you'll probably need to install the [XQuartz](https://www.xquartz.org/) package.