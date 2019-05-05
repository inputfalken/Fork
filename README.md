# Fork

## Description

An interactive process handler.

## Supported commands

* Restart {ALIAS}
* Start {ALIAS}
* Stop {ALIAS}
* List
* Exit

## Setup

Create a file in the home directory with the name `.fork.json`. with a structure that matches the example json below.

```
[
  {
    "alias": "MyApplication",
    "tasks": [
      {
        "workingDirectory": "C:\\Users\\{USER}\\application\\backend\\",
        "fileName": "dotnet",
        "arguments": "run",
        "alias": "Backend"
      },
      {
        "workingDirectory": "C:\\Users\\{USER}\\application\\frontend\\",
        "fileName": "npm",
        "arguments": "run serve",
        "alias": "Frontend",
        "useSeperateWindow": true
      }
    ]
  }
]
```
