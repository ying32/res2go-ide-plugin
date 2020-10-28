图文说明：https://gitee.com/ying32/govcl/wikis/pages?sort_id=2645001&doc_id=102420  

#### v1.0.13

* 1、修改：非Windows下强制使用CGO选项。
* 2、增加：可自定义`GoPATH`变量路径。

#### v1.0.12

* 1、修改：非Windows下不再转换资源，因为没有windres这个东西。
* 2、修复：非Windows下编译不成功。
* 3、修复：非Windows下默认启用CGO选项。

#### v1.0.11

* 1、修复保存设置时会弹出错误问题。
* 2、增加：如在GoPath中创建工程，如果包名不为main则导入以GoPath为路径的包名，反之导入相对包名。

#### v1.0.10

* 1、修复`Go Tags`在某些情况下会丢失。
* 2、res2go选项中增加`Go Build mode`选择。

#### v1.0.9

* 1、优化解析lpr文件，不再直接读取lpr文件。
* 2、增加：`Project -> Project Options ... -> Compiler Options -> Debugging -> Generate info for the debugger`对应go ldflags `-w`。调试符信息成控制。
* 3、增加：`Project -> Project Options ... -> Compiler Options -> Debugging -> Other debugging info -> Strip symbols form executable`对应go ldflags `-s`。控制移除符号信息。
* 4、增加：res2go选中增加`Enabled CGO`选项。

#### v1.0.8

* 1、增加`tempdll`约束选项，Windows与Linux下开启后可将liblcl打包到可执行文件中。
* 2、增加`finalizerOn`约束选项，开启后非组件类可不调用`Free`方法。
* 3、增加设置`tags`约束选项。

#### v1.0.7

* 1、增加Lazarus IDE中直接设置应用程序标题"Application.SetTitle"。
* 2、增加"使用默认winappres"包选项。
* 3、如果设置中"PackageName"指定了其它包名，如果main.go中未导入，则导入相对路径的包名。
* 4、拦截Lazarus的编译功能，更改为go的编译行为。
* 5、工程选项中`Compiler Options -> Config and Target -> Win32 gui application (-WG)`可控制去除命令行窗口。
* 6、`Project -> Run Parameters -> Command line parameters`可控制运行时传入的命令行。
* 7、工程选项中`Compiler Options -> Paths -> Target file name(-o)`可控制生成的可执行文件名。
* 8、工程选项中`Compiler Options -> Config and Target -> Target platform`可以控制`GOOS`(Target OS)和`GOARCH`(Target CPU family)变量。

#### v1.0.6

* 1、默认使用fpc中自带的windres，如果未找到则使用`PATH`中的windres。
* 2、执行windres出错则输出错误提示。
* 3、修复未启用转换也会转换res文件。

#### v1.0.5

* 1、OutPath支持相对路径、绝对路径、Lazarus IDE宏。
* 2、修复main.go文件创建时没有添加"vcl.Application.SetMainFormOnTaskBar(true)"语句。
* 3、main.go中添加"vcl.Application.SetScaled"语句，其值取自工程选项中"Use LCL scaling(Hi-DPI)"。
* 4、支持自定义go的包名（需要自己在main.go中添加包的导入）。

#### v1.0.4

* 1、增加利用Lazarus IDE完成Windows可执行文件的图标、Manifest、版本信息修改。
* 2、main.go不再添加"winappres"包。

#### v1.0.3

* 1、增加保存“gfm”文件选项，默认不保存。
* 2、预留输出"Rust", "Nim"语言选项（未开始编写代码）。

#### v1.0.2

* 1、重构全部的转换功能，不再读取lfm文件进行转换了，事件上利用RTTI转换参数。
* 2、增加“使用原始单元”选中功能。

#### v1.0.1

* 1、修复打开工程时不加载配置问题。