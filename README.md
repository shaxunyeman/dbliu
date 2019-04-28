# Build on ARM

## 版本要求

	1. arm 交叉编译工具最低版本是 4.8.3
 	2. 支持的交叉编译工具链有 a7,a9 和 cgel

## 编译指令	

```bash
# zebra 源码根目录
> ./script/compile-arm.sh build_path tool_path cross_prefix frame boost_ver build_type
```

- build_path

  zebra 编译目录

- tool_path 

  交叉编译工具目录

- cross_prefix

  交叉编译工具前缀名称

- ARM 版本

  目前支持的版本有 a7,a9 和 cgel

- boost_ver

  使用交叉工具编译 boost 库时，需要根据交叉编译工具的版本生成相关 boost 编译目录，boost_ver 格式为 `boost_1_57_0_GNU_4_8_2`，其中 4_8_2 为交叉编译工具的版本，boost_1_57_0_GNU 为 boost 库版本

- build_type

  Debug 或者 Release

## 编译例子

```bash
# zebra 源码根目录下执行
> ./script/compile-arm.sh ~/work/build_release_arm/ /home/dbliu/work/tools/arm-bcm2708/arm-rpi-4.9.3-linux-gnueabihf arm-linux-gnueabihf- a9 boost_1_57_0_GNU_4_9_3 Release
```

​	执行成功后，在编译目录 `~/work/build_release_arm/` 会生成 `peersafe_server` 执行程序；如果要生成 `peersafe_client` 程序，在编译目录下执行 `make peersafe_client -j2` 指令
