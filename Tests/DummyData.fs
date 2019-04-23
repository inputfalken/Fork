module Tests.DummyData
open ProcessHandler

let groupAlias = "foo"
let processAlias1 = "testing"
let processAlias2 = "testing2"
let startinfo = [
    {
       Processes = [
           {
               Alias = processAlias1
               Arguments = {
                    WorkingDirectory = "";
                    FileName = ""
                    Arguments = ""
                    Alias = ""
                    UseSeperateWindow = true
               }
               Process = null
           }
           {
               Alias = processAlias2
               Arguments = {
                    WorkingDirectory = "";
                    FileName = ""
                    Arguments = ""
                    Alias = ""
                    UseSeperateWindow = true
               }
               Process = null
           }
       ]
       Alias = groupAlias
    }
 ]
