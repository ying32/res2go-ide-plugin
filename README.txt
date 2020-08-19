图文说明：https://gitee.com/ying32/govcl/wikis/pages?sort_id=2645001&doc_id=102420

v1.0.7

  1、增加Lazarus IDE中直接设置应用程序标题"Application.SetTitle"。

v1.0.6

  1、默认使用fpc中自带的windres，如果未找到则使用`PATH`中的windres。
  2、执行windres出错则输出错误提示。
  3、修复未启用转换也会转换res文件。

v1.0.5

  1、OutPath支持相对路径、绝对路径、Lazarus IDE宏。
  2、修复main.go文件创建时没有添加"vcl.Application.SetMainFormOnTaskBar(true)"语句。
  3、main.go中添加"vcl.Application.SetScaled"语句，其值取自工程选项中"Use LCL scaling(Hi-DPI)"。
  4、支持自定义go的包名（需要自己在main.go中添加包的导入）。

v1.0.4

  1、增加利用Lazarus IDE完成Windows可执行文件的图标、Manifest、版本信息修改。
  2、main.go不再添加"winappres"包。

v1.0.3

  1、增加保存“gfm”文件选项，默认不保存。
  2、预留输出"Rust", "Nim"语言选项（未开始编写代码）。

v1.0.2
 
  1、重构全部的转换功能，不再读取lfm文件进行转换了，事件上利用RTTI转换参数。
  2、增加“使用原始单元”选中功能。

v1.0.1
  
  1、修复打开工程时不加载配置问题。