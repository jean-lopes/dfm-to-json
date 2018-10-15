# Delphi Form File (DFM) to JSON converter

Convert a DFM to JSON, for more information issue --help command

You can download the [latest release here](https://github.com/jean-lopes/dfm-to-json/releases/latest).

You can check the EBNF [here](https://jean-lopes.github.io/dfm-to-json/).

Consider this DFM [resources/snippet.dfm](resources/snippet.dfm):
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
produces the following [resources/snippet.json](resources/snippet.json):
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

A more complete sample is available in the resources folder: [resources/sample.dfm](resources/sample.dfm) -> [resources/sample.json](resources/sample.json).

## Building from source

### Requirements
- [git](https://git-scm.com/)
- [stack](https://docs.haskellstack.org)

### Commands
```bash
$ git clone https://github.com/jean-lopes/dfm-to-json.git
$ cd dfm-to-json
$ stack install
$ dfm-to-json --version
```

If you can't execute the last command, it means you don't have the default `stack install` directory in your environment `PATH` variable. check [local-bin-path](https://docs.haskellstack.org/en/v1.5.1/yaml_configuration/#local-bin-path)

Tip for windows users: default `stack` local-bin-path is: `%APPDATA%\local\bin`


