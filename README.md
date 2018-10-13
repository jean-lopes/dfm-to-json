# Delphi Form File (DFM) to JSON converter

Convert a DFM to JSON, for more information issue --help command

Consider this DFM [snippet](resources/snippet.dfm):
```pascal
object Form1: TForm1
  Left = 192
  Top = 107
  Width = 463
  Height = 224
end
```
Executing the command:
```bash
$ dfm-to-json resources/snippet.dfm
```
produces the following [JSON](resources/snippet.json):
```json
{
  "name": "Form1",
  "type": "TForm1",
  "properties": {
    "height": 224,
    "left": 192,
    "width": 463,
    "top": 107
  },
  "objects": []
}
```

A more complete sample is available in the resources folder: [sample.dfm](resources/sample.dfm) -> [sample.json](resources/sample.json).