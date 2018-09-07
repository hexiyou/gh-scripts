'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'                           Windows Environment Setup Tool                                    '
'                                @author:Lonelyer                                             ' 
'                             @Email: hexiyou.cn@gmail.com                                    '
'                                  @date:2015-06                                              '
'                                                                                             '
'       description£ºAdd or Remove Path Environment or setup Others Environment variables.    '
'                   (Java Environment etc..)                                                  '
'                                                                                             '
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''


Dim Argvc,sysEnv,userEnv,quietMode,fsoObject,shell,argDictionary,java_home
Const TristateUseDefault = -2

quietMode = False

Function Init()
    If VarType(sysEnv)=0 Then Set sysEnv = CreateObject("WScript.Shell").Environment("SYSTEM")
    If VarType(userEnv)=0 Then Set userEnv = CreateObject("WScript.Shell").Environment("USER")
End Function 


Function InitFso()
    If VarType(fsoObject)=0 Then Set fsoObject = CreateObject("scripting.filesystemobject")
    If VarType(Shell)=0 Then Set Shell = CreateObject("wscript.Shell")
End Function


Function Destory()
    If Not sysEnv Is Nothing Then Set sysEnv = Nothing
    If Not userEnv Is Nothing Then Set userEnv = Nothing
End Function


Sub MainEnter()
    Dim Args,CmdArr,i,keyIndex
    Call InitFso()
    Argvs = ArgumentsToString()
    host = WScript.FullName
	If LCase( right(host, len(host)-InStrRev(host,"\")) ) = "wscript.exe" Then
	    Shell.run "cscript """ & WScript.ScriptFullName & chr(34), 0
	    WScript.Quit
	End If

    
    Set argDictionary = CreateObject("Scripting.Dictionary")
    CmdArr = Split(Argvs,"|")
    keyIndex = 0
    For i=0 To UBound(CmdArr)
        If CmdArr(i)<>"-q" And CmdArr(i)<>"-quiet" Then
            argDictionary.Add keyIndex,CmdArr(i)
            keyIndex = keyIndex+1
        End If
    Next
    Call Main()
End Sub



Function Main()
	Dim jdkInfo,batrun,Shell,Read
    Call Init()
    jdkInfo = fsoObject.GetFolder(".").Path & "\jdk.txt"
    batrun = fsoObject.GetFile("getjdkpath.bat").Path
    Set Shell = CreateObject("wscript.Shell")
    Shell.Run batrun & " continue",vbhide,True
    Set Read=Shell.Exec("cmd /c type " & jdkinfo)
    java_home=Read.StdOut.ReadLine
    sysEnv.Item("JAVA_HOME")=java_home
    Call AddPath(java_home & "\bin")
    Call AddPath(java_home & "\jre\bin")
    sysEnv.Item("ClassPath")=".;%JAVA_HOME%\lib\dt.jar;%JAVA_HOME%\lib\tools.jar"
    MsgBox "Execute Done...",64,"Message"
End Function

' Method£ºAdd String To %PATH%
Sub AddPath(pathStr)
    Call Init()
    Dim oldPath,newPath,perAddPath
    oldPath = sysEnv.Item("PATH")
    perAddPath = pathStr
    ' check the path is exists or not,if exists do exit
    If CheckPathHasExists(oldPath,pathStr)=False Then
    newPath = oldPath
    If HasSemi(oldPath,"end")=False And HasSemi(pathStr,"begin")=False Then newPath = newPath & ";"
    'Add path at last
    newPath = newPath & perAddPath
    sysEnv.Item("PATH") = newPath
    'Call PrintMsg("The Path added to the %PATH% Environment." & vbCrLf & "+" & Space(8) & perAddPath, False)
    End If
End Sub

''
'' parse CommandLine action when only one option
Sub parseSingleParam(args)
    Select Case args
        Case "-h","/?","--help"
        Call PrintUsage("Script example£º")
        Case "-export","--export"
        Call ExportPath("pathenv_export.txt")
        Case "-about"
        Call showAbout()
        Case "-outhelp"
        Call ExportHelpInfo("setenv_help.txt")
        Case Else
        Call SinglePath(args)
        'Call PrintMsg("Undefined commandline option " & args, True)
    End Select
End Sub


'
'' description £º insert path string at %PATH% beginning
Sub InsertPath(pathStr)
    Call Init()
    Dim oldPath,newPath,perAddPath
    oldPath = sysEnv.Item("PATH")
    perAddPath = pathStr
    ' check the path is exists or not,if exists do exit
    If CheckPathHasExists(oldPath,pathStr)=True Then Call PrintMsg("The Path has Exists, script be quit now!",True)
    newPath = oldPath
    If HasSemi(oldPath,"begin")=False And HasSemi(perAddPath,"end")=False Then newPath = ";" & newPath
    'Add path at last
    newPath = perAddPath &  newPath 
    'MsgBox newPath 
    sysEnv.Item("PATH") = newPath
    Call PrintMsg("The Path inserted to the %PATH% Environment Now." & vbCrLf & Space(8) & perAddPath & " +", True)
End Sub

Sub RemovePath(pathStr)
    Dim tmpReg
    Call Init()
    Dim oldPath,newPath
    oldPath = sysEnv.Item("PATH")
    Set tmpReg = New RegExp
    pathStr = Replace(pathStr, "\","\\")
    pathStr = Replace(pathStr, ",","\,")
    pathStr = Replace(pathStr, ".","\.")
    pathStr = Replace(pathStr, "-","\-")
    pathStr = Replace(pathStr, "_","\_")
    pathStr = Replace(pathStr, "+","\+")
    pathStr = Replace(pathStr, ":","\:")
    pathStr = Replace(pathStr, "[","\[")
    pathStr = Replace(pathStr, "]","\]")
    pathStr = Replace(pathStr, "(","\(")
    tmpReg.Pattern = ";?" & pathStr & "(;|$)"
    tmpReg.IgnoreCase = True
    tmpReg.Global = True
    newPath = tmpReg.Replace(oldPath,"")
    sysEnv.Item("PATH") = newPath
    Call PrintMsg("The Path has be removed Now." & vbCrLf & Space(8) & perAddPath, True)
End Sub

'check the path is a file or a directory
Sub SinglePath(pathStr)
    Call InitFso()
    If fsoObject.FolderExists(pathStr)=True Then
        Call AddPath(pathStr)
    ElseIf fsoObject.FileExists(pathStr)=True Then
        'if target is a file,Join parentfoldpath to PATH 
        Call AddPath(fsoObject.GetParentFolderName(pathStr))
    Else
        Call PrintMsg("missing params or undefined error,please check commandline option! ", True)     
    End If
End Sub



Sub ExportPath(filePath)
    Call Init()
    Call InitFso()
    Set file = fsoObject.OpenTextFile(filePath,2,True, TristateUseDefault)
    file.WriteLine "################## SYSTEM PATH Below ###################"
    file.Write sysEnv.Item("PATH")
    file.WriteBlankLines 1
    file.WriteLine "################### USER PATH Below ####################"
    file.Write userEnv.Item("Path")
    file.WriteLine "################### End OutPut ####################"
    file.Close
    PrintMsg "PATH Environment be exported to the file!" & vbCrLf & " - " & filePath,True
    Set File = Nothing
End Sub

Sub ExportHelpInfo(helpFile)
    Call InitFso()
    Set file = fsoObject.OpenTextFile(helpFile,2,True, TristateUseDefault)
    file.Write "Usage: " &vbCrLf &_
    "(FilePath|FolderPath) Add file or folder Absloute path to %PATH% Var,support mouse drag operation" & vbCrLf & vbCrLf &_
    "-add, -append	[Path] Add directory Path To %PATH% Var" & vbCrLf & vbCrLf &_
    "-remove, -del	[Path] Add directory Path From %PATH% Var" &vbCrLf & vbCrLf &_
    "-insert, -prepend	[Path] Insert directory Path To %PATH% Var at The Beginging of the value" & vbCrLf & vbCrLf & _
    "-query	Path Query path string in %PATH% Var,this operation allow use wildcard (* and ?)" & vbCrLf & vbCrLf & _
    "-export  [Filename|FilePath]   Export the %PATH% Var to one file.(include USER Environment)" & vbCrLf & vbCrLf & _
    "$ENV_Item  Value   Set value for other Environment VARS except %PATH% var" & vbCrLf & vbCrLf & _
    "-read, --read  $ENV_Item   Read value of Environment VARS include %PATH% var" & vbCrLf & vbCrLf & _
    "-h, --help, /?   Show this help message box"
    file.Close
    PrintMsg "Help message be exported to the file!" & vbCrLf & " - " & helpFile,True
    Set File = Nothing
End Sub


Sub ReadEnv(envItem)
    Call Init()
    Call PrintMsg(sysEnv.Item(envItem), False)
    '''--------------- print user env,by user custom
    'Call PrintMsg(userEnv.Item(envItem), True)
End Sub


Sub showAbout()
    PrintMsg "This is an Environment Setting Tool use VBScript programing" & vbCrLf & vbCrLf & _
    Space(58) & "- Author: lonely" & vbCrLf & _
    Space(45) & "- Email: hexiyou.cn@gmail.com " & vbCrLf & _
    Space(56) & "- Date: 2015-06-15", _
    True
End Sub


Function CheckPathHasExists(EnvPath,pathStr)
    Set tmpReg = New RegExp
    pathStr = Replace(pathStr, "\","\\")
    pathStr = Replace(pathStr, ",","\,")
    pathStr = Replace(pathStr, ".","\.")
    pathStr = Replace(pathStr, "-","\-")
    pathStr = Replace(pathStr, "_","\_")
    pathStr = Replace(pathStr, "+","\+")
    pathStr = Replace(pathStr, ":","\:")
    pathStr = Replace(pathStr, "[","\[")
    pathStr = Replace(pathStr, "]","\]")
    pathStr = Replace(pathStr, "(","\(")
    pathStr = Replace(pathStr, ")","\)")
    tmpReg.Pattern = "" & pathStr & "(;|$)"
    tmpReg.Global = True
    tmpReg.IgnoreCase = True
    CheckPathHasExists = tmpReg.Test(EnvPath)
    Set tmpReg = Nothing
End Function

Function checkSinglePath(pathStr)
    Set tmpReg = New RegExp
    tmpReg.Pattern = "^[^;]+$"
    tmpReg.Global = True
    checkSinglePath = tmpReg.Test(pathStr)
    Set tmpReg = Nothing
End Function

Function HasSemi(Str,Pos)
    Set tmpReg = New RegExp
    Select Case LCase(Pos)
        Case "end"
        TmpReg.Pattern = ";$"
        Case "begin"
        TmpReg.Pattern = "^;"
        Case Else
        TmpReg.Pattern = "^.*([.]+;)+;$"
    End Select
    tmpReg.Global = True
    tmpReg.IgnoreCase = True
    HasSemi = TmpReg.Test(Str)
    Set tmpReg = Nothing
End Function


Function ArgumentsToString()
    Dim tmpArr(),i
    i=0
    For Each Arg In WScript.Arguments
        ReDim Preserve tmpArr(i)
        tmpArr(i)=Arg
        i=i+1
    Next
    ArgumentsToString = Join(tmpArr,"|")
End Function


Sub PrintUsage(Msg)
    MsgBox Msg & vbCrLf & "Usage: " &vbCrLf &_
    "(FilePath|FolderPath) Add file or folder Absloute path to %PATH% Var,support mouse drag operation" & vbCrLf & vbCrLf &_
    "-add, -append	[Path] Add directory Path To %PATH% Var" & vbCrLf & vbCrLf &_
    "-remove, -del	[Path] Add directory Path From %PATH% Var" &vbCrLf & vbCrLf &_
    "-insert, -prepend	[Path] Insert directory Path To %PATH% Var at The Beginging of the value" & vbCrLf & vbCrLf & _
    "-query	Path Query path string in %PATH% Var,this operation allow use wildcard (* and ?)" & vbCrLf & vbCrLf & _
    "-export  [Filename|FilePath]   Export the %PATH% Var to one file.(include USER Environment)" & vbCrLf & vbCrLf & _
    "$ENV_Item  Value   Set value for other Environment VARS except %PATH% var" & vbCrLf & vbCrLf & _
    "-read, --read  $ENV_Item   Read value of Environment VARS include %PATH% var" & vbCrLf & vbCrLf & _
    "-h, --help, /?   Show this help message box" _
    ,64,"Usage"
End Sub


Sub PrintMsg(Msg,isExit)
    If Not quietMode Then
        MsgBox Msg,64,"Message Tip"
    Else
        WScript.Echo Msg
    End If
    If isExit=True Then WScript.Quit
End Sub


Call MainEnter()