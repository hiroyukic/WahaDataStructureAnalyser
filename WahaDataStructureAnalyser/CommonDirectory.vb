

Public Class CommonDirectory


    'ビューフィルタ種別名称・ディクショナリー
    Private dic = New Dictionary(Of String, String)

    '関数キーワード
    Const FUNCT As String = "FUNCT"
    Const OPE As String = "OPERATOR"

    Const VIEW_FILTER_CLASS As String = "VIEW_FILTER_CLASS"

    'オペレータ演算子・ディクショナリー
    'Private DIC_OPERATOR_NAME = New Dictionary(Of String, String)
    'Private dicVIEW_FILTER_NAME = New Dictionary(Of String, String)


    'Private ENUMTAG = New EnumTag()



    ' タイトル行 _
    Public Const COL_TITLE As String = "SeqNo," _
            & "タイムスタンプ," _
            & "格納フォルダ（業務名）," _
            & "WTJファイル名," _
            & "ビューフィルタ種別," _
            & "ビューフィルタ種別名称," _
            & "ビューフィルタ名称," _
            & "移送先ビューID," _
            & "移送先ビューテーブル名称," _
            & "移送先カラムID," _
            & "移送先カラム長," _
            & "移送先カラム名称," _
            & "移送元ビューID," _
            & "移送元ビューテーブル名称," _
            & "移送元カラムID," _
            & "移送元カラム長," _
            & "移送元カラム名称," _
            & "移送値型," _
            & "移送値," _
            & "関数タイプ," _
            & "直送フラグ," _
            & "抽出条件（条件値）," _
            & "抽出条件（比較演算子）," _
            & "抽出条件（項目）," _
            & "WorkDir," _
            & "処理（0:前／1:後),"

    Private _seqNo As String                ' "SeqNo,"
    Private _timeStamp As String            ' "タイムスタンプ," _
    Private _parentDir As String            ' "格納フォルダ（業務名）,"
    Private _fileNameBody As String         ' "WTJファイル名,"
    Private _vFilterKind As String          ' "ビューフィルタ種別,"
    Private _vFilterKindName As String      ' "ビューフィルタ種別名称,"
    Private _vFilterName As String          ' "ビューフィルタ名称,"

    Private _distViewID As String          ' "移送先ビューID," 
    Private _distView As String            ' "移送先ビューテーブル名称," _
    Private _distColId As String            ' "移送先カラムID," _
    Private _distColLength As String        ' "移送先カラム長," _
    Private _distColName As String          ' "移送先カラム名称," _

    Private _orgViewID As String           ' "移送元ビューID," 
    Private _orgView As String             ' "移送元ビューテーブル名称," _
    Private _orgColId As String             ' "移送元カラムID," _
    Private _orgColLength As String         ' "移送元カラム長," _
    Private _orgColName As String           ' "移送元カラム名称," _

    Private _valueType As String            ' "移送値型," _
    Private _value As String                ' "移送値," _
    Private _functionType As String         ' "関数タイプ," _
    Private _directFlg As String            ' "直送フラグ," _

    Private _selectConditionVal As String   ' "抽出条件（条件値）," _	
    Private _selectConditionOpe As String   ' "抽出条件（比較演算子）,"
    Private _selectConditionAttr As String  ' "抽出条件（項目）," _
    Private _workDir As String              ' "WorkDir," _
    Private _prcessing As String            ' "処理（0:前／1:後),"



    Sub New()
        'ビューフィルタ・区分名称 
        Me.dic.add(VIEW_FILTER_CLASS & "_" & "0", "標準")
        Me.dic.add(VIEW_FILTER_CLASS & "_" & "1", "結合")
        Me.dic.add(VIEW_FILTER_CLASS & "_" & "2", "統合")
        Me.dic.add(VIEW_FILTER_CLASS & "_" & "3", "集計")
        Me.dic.add(VIEW_FILTER_CLASS & "_" & "4", "分割")
        Me.dic.add(VIEW_FILTER_CLASS & "_" & "5", "参照")
        Me.dic.add(VIEW_FILTER_CLASS & "_" & "6", "縦展開")
        Me.dic.add(VIEW_FILTER_CLASS & "_" & "7", "更新")

        'オペレータ区分
        Me.dic.add(OPE & "_" & "0", "＝")

        Me.dic.add(OPE & "_" & "1", "≠")
        Me.dic.add(OPE & "_" & "2", "＜")
        Me.dic.add(OPE & "_" & "3", "＞")
        Me.dic.add(OPE & "_" & "4", "≦")
        Me.dic.add(OPE & "_" & "5", "≧")
        Me.dic.add(OPE & "_" & "6", "中間一致")
        Me.dic.add(OPE & "_" & "7", "前方一致")
        Me.dic.add(OPE & "_" & "8", "後方一致")

        'Waha関数
        Me.dic.add(FUNCT & "_" & "0", "")
        Me.dic.add(FUNCT & "_" & "1", "Concat")
        Me.dic.add(FUNCT & "_" & "2", "SearchB")
        Me.dic.add(FUNCT & "_" & "3", "Search")
        Me.dic.add(FUNCT & "_" & "4", "SubstrB")
        Me.dic.add(FUNCT & "_" & "5", "Substr")
        Me.dic.add(FUNCT & "_" & "6", "Replace")
        Me.dic.add(FUNCT & "_" & "7", "Trim")
        Me.dic.add(FUNCT & "_" & "8", "Rtrim")
        Me.dic.add(FUNCT & "_" & "9", "LTrim")
        Me.dic.add(FUNCT & "_" & "10", "ToUpper")
        Me.dic.add(FUNCT & "_" & "11", "ToLower")
        Me.dic.add(FUNCT & "_" & "12", "ToUpperMB")
        Me.dic.add(FUNCT & "_" & "13", "ToLoweMB")
        Me.dic.add(FUNCT & "_" & "14", "ToMB")
        Me.dic.add(FUNCT & "_" & "15", "ToSB")
        Me.dic.add(FUNCT & "_" & "16", "Abs")
        Me.dic.add(FUNCT & "_" & "17", "Round")
        Me.dic.add(FUNCT & "_" & "18", "Trunc")
        Me.dic.add(FUNCT & "_" & "19", "Decode")
        Me.dic.add(FUNCT & "_" & "20", "IF")
        Me.dic.add(FUNCT & "_" & "21", "Sum")
        Me.dic.add(FUNCT & "_" & "22", "Diff")
        Me.dic.add(FUNCT & "_" & "23", "Mult")
        Me.dic.add(FUNCT & "_" & "24", "Div")
        Me.dic.add(FUNCT & "_" & "25", "Mod")
        Me.dic.add(FUNCT & "_" & "26", "26")
        Me.dic.add(FUNCT & "_" & "27", "String2Number")
        Me.dic.add(FUNCT & "_" & "28", "Number2String")
        Me.dic.add(FUNCT & "_" & "29", "Len")
        Me.dic.add(FUNCT & "_" & "30", "LenB")
        Me.dic.add(FUNCT & "_" & "31", "EoDate")
        Me.dic.add(FUNCT & "_" & "32", "SubDate")
        Me.dic.add(FUNCT & "_" & "33", "FormatRead")
        Me.dic.add(FUNCT & "_" & "34", "FormatWrite")
        Me.dic.add(FUNCT & "_" & "35", "Date2Number")
        Me.dic.add(FUNCT & "_" & "36", "Number2Date")
        Me.dic.add(FUNCT & "_" & "37", "String2Date")
        Me.dic.add(FUNCT & "_" & "38", "Date2String")
        Me.dic.add(FUNCT & "_" & "39", "39")
        Me.dic.add(FUNCT & "_" & "40", "Numberring")
        Me.dic.add(FUNCT & "_" & "41", "Element")
        Me.dic.add(FUNCT & "_" & "42", "Left")
        Me.dic.add(FUNCT & "_" & "43", "LeftB")
        Me.dic.add(FUNCT & "_" & "44", "Right")
        Me.dic.add(FUNCT & "_" & "45", "RightB")
        Me.dic.add(FUNCT & "_" & "46", "Separate")
        Me.dic.add(FUNCT & "_" & "47", "SeparateB")
        Me.dic.add(FUNCT & "_" & "48", "Repeat")
        Me.dic.add(FUNCT & "_" & "49", "Now")
        Me.dic.add(FUNCT & "_" & "50", "User")
        Me.dic.add(FUNCT & "_" & "51", "RowCount")
        Me.dic.add(FUNCT & "_" & "52", "CodeConvert")
        Me.dic.add(FUNCT & "_" & "53", "Counter")
        Me.dic.add(FUNCT & "_" & "54", "BoDate")
        Me.dic.add(FUNCT & "_" & "55", "CreateDate")
        Me.dic.add(FUNCT & "_" & "56", "And")
        Me.dic.add(FUNCT & "_" & "57", "Or")
        Me.dic.add(FUNCT & "_" & "58", "Not")
        Me.dic.add(FUNCT & "_" & "59", "Bool")
        Me.dic.add(FUNCT & "_" & "60", "LogIf")
        Me.dic.add(FUNCT & "_" & "61", "61")
        Me.dic.add(FUNCT & "_" & "62", "62")
        Me.dic.add(FUNCT & "_" & "63", "ToKanaUpperMB")
        Me.dic.add(FUNCT & "_" & "64", "64")
        Me.dic.add(FUNCT & "_" & "65", "IfError")
        Me.dic.add(FUNCT & "_" & "66", "RaiseError")
        Me.dic.add(FUNCT & "_" & "67", "RaiseWarning")
        Me.dic.add(FUNCT & "_" & "68", "68")
        Me.dic.add(FUNCT & "_" & "69", "PreValue")
        Me.dic.add(FUNCT & "_" & "70", "MatchRE")
        Me.dic.add(FUNCT & "_" & "71", "ReplaceRE")
        Me.dic.add(FUNCT & "_" & "72", "DateDiff")
        Me.dic.add(FUNCT & "_" & "73", "DateSum")
        Me.dic.add(FUNCT & "_" & "74", "Exec")
        Me.dic.add(FUNCT & "_" & "75", "Binary2String(code指定有)")
        Me.dic.add(FUNCT & "_" & "76", "Binary2String(code指定無)")
        Me.dic.add(FUNCT & "_" & "77", "Supply")


        Me.dic.add("COLUMN_LENGTH_0_0", "")
        Me.dic.add("COLUMN_0_0", "")
        Me.dic.add("VIEW_0", "")
    End Sub





    ' キー Key1
    Public Function add(key As String, val As String) As Boolean
        Try
            Me.dic.add(key, val)
            Return True
        Catch ex As Exception
            Debug.Print("CommonDictionary.add[exception] : key=" & key & "  val=" & val)
            Return False
        End Try
    End Function

    ' キー Key1
    Public Function GetAttr(pkey As String) As String

        '属性名称を取得
        Dim val As String
        Try
            val = dic(pkey)
            If val = Nothing Then
                Return ""
            End If
            Return val
        Catch ex As Exception
            Debug.Print("CommonDictionary.GetAttr[exception] : key=" & pkey)
            Return ""
        End Try

    End Function


    ' 複合キー Key1, Key2
    Public Function SetAttr(pkey1 As String, pkey2 As String, pval As String) As Boolean
        Dim key As String = pkey1 & "_" & pkey2
        add(key, pval)
        'Try
        '    add(key, pval)
        '    Return True
        'Catch ex As Exception
        '    Debug.Print("CommonDictionary.SetAttr[exception] : key=" & key)
        '    Return False
        'End Try
    End Function



    ' 複合キー Key1, Key2 
    Public Function GetAttr(pkey1 As String, pkey2 As String) As String
        Dim key As String = pkey1 & "_" & pkey2
        Return GetAttr(key)
    End Function



    ' 複合キー Key1, Key2, Key3
    Public Function SetAttr(pkey1 As String, pkey2 As String, pkey3 As String, pval As String) As Boolean
        Dim key As String = pkey1 & "_" & pkey2 & "_" & pkey3
        add(key, pval)
        'Try
        '    add(key, pval)
        '    Return True
        'Catch ex As Exception
        '    Return False
        'End Try
    End Function



    ' 複合キー Key1, Key2, Key3
    Public Function GetAttr(pkey1 As String, pkey2 As String, pkey3 As String) As String
        Dim key As String = pkey1 & "_" & pkey2 & "_" & pkey3
        Dim val = ""
        Try
            val = GetAttr(key)
        Catch ex As Exception
        End Try

        Return val

    End Function




    Public Sub print()

        'ループ処理する
        For Each itm As KeyValuePair(Of String, String) In Me.dic
            Debug.Print(itm.Key & "=" & itm.Value)
        Next

    End Sub



End Class
