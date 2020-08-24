# BilibiliDanmakuParser

获取Bilibili的视频弹幕数据。

由于程序用的是暴力枚举 Uid ，程序运行速度会比较慢，同时会占用部分磁盘空间（大约 6GiB）。

需要 [`.NET Core 3.1`](https://dotnet.microsoft.com/download/dotnet-core/3.1) 。

## 生成

``` bash
dotnet build -c:Release
```

## 运行

``` bash
./BilibiliDanmakuParser 命令 参数
```

命令及参数说明请运行通过以下命令运行程序查看。

``` bash
./BilibiliDanmakuParser
```