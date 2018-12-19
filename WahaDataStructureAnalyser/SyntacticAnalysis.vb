'----------------------------------------------------
'wtjの構文解析クラス
' 解析対象のファイルパスを受けて、構文解析を行い、結果を出力
'----------------------------------------------------

Imports System.Text
Imports System.Text.RegularExpressions
Imports Microsoft.VisualBasic.FileIO

Public Class SyntacticAnalysis

    'デバッグ・フラグ （タグキーワード＋ファイル内行番号） 
    Const DBGFLG As Boolean = 1

    '出力ファイル（CSVファイル）
    Private OUT_FILE As String

    Private intRowCount As Integer = 0

    Private FilePath As String
    '全ファイル共通カウンタ
    Private intTotalLineCount As UInteger = 0

    'キーワード＆プログラム・でぃくしょな りー 
    Private dict As CommonDirectory

    ' Directory用・関数キーワード
    Const FUNCT As String = "FUNCT"
    Const OPE As String = "OPERATOR"
    Const VIEW_FILTER_CLASS As String = "VIEW_FILTER_CLASS"

    'ファイルの生成日時
    Private FILE_CREAT_TIME As String
    'ファイルの格納ディレクトリ  ※直近親ディレクトリ

    '出力属性情報クラス
    Private RECORD As StructRecord

    'ビューフィルタ構造体 　※'[VFilter] [CFilter] [CFValue]
    Private Structure STRUCT_VIEW_FILTER
        Public AtrributeList As String()          ' 属性のリスト配列
        '[VFilter] ビューフィルタ定義
        Public VF1Kind As Integer           'ビューフィルタ区分
        Public VF1KindName As String        'ビューフィルタ区分名称
        Public VF1FilterId As Integer             '
        Public VF1FilterName As String            '
        '[CFilter] 移送先・属性ＩＤ
        Public CF2FiltererOutViewId As Integer    '
        Public CF2FilterOutColId As Integer       '
        '[CFValue]
        Public CFV3ValueViewID As Integer         ' 移送元・ビューID
        Public CFV3ValueValue As String           ' 移送元・移送値
        Public CFV3ValueSFType                    ' 関数タイプ
        Public CFV3ValueRealValueType             ' 実値型
        Public CFV3ValuelikColID                  ' 移送元・カラムID
        Public CFV3ValueID                        ' a
        Public CFV3ValueIsRoot                    ' ルートフラグ
        Public CFV3ValueChildIDs As String        ' パラメータ
        '二項比較子
        Public CFV3ValueValueType As String       'ValueType
        Public CFV3ValueSetValueType As String    'SetValueType
        Public CFV3ValueObjType As String         'ObjType
        '
        Public CFV3ValueName As String            '
    End Structure

    Private recordViewFilter As STRUCT_VIEW_FILTER

    ''' -----------------------------------------------------------------------------------
    ''' タグ・キーワード  Enum
    ''' -----------------------------------------------------------------------------------
    Private Enum Status
        iView
        iColumn
        iVFilter
        iCFilter
        iCFValue
        iSelectCondition
        iDViewSelectCondition
        iDeleteSelectContdition
        iRepository
        iProcess
        iViewInfo
        iLookupViewInfo
        iSquareBracket
        iNone
    End Enum

    ''' -----------------------------------------------------------------------------------
    ''' タグ・キーワード  ストリングパターン
    ''' -----------------------------------------------------------------------------------
    Private regPatternView As String = "^\[View\]"
    Private regPatternColumn As String = "^\[Column\]"
    Private regPatternVFilter As String = "^\[VFilter\]"
    Private regPatternCFilter As String = "^\[CFilter\]"
    Private regPatternCFValue As String = "^\[CFValue\]"
    Private regPatternSelectCondition As String = "^\[SelectCondition\]"
    Private regPatternDViewSelectCondition As String = "^\[DViewSelectCondition\]"
    Private regPatternDeleteSelectContdition As String = "^\[DeleteSelectCondition\]"
    Private regPatternRepository As String = "^\[Repository\]"
    Private regPatternProcess As String = "^\[Process\]"
    Private regPatternViewInfo As String = "^\[ViewInfo\]"
    Private regPatternLookupViewInfo As String = "^\[LookupViewInfo\]"
    Private regPatternSquareBracket As String = "^\["
    ''' -----------------------------------------------------------------------------------
    ''' タグ・キーワード 用 RegulerExpretion
    ''' -----------------------------------------------------------------------------------
    Private rxView = New Regex(regPatternView, RegexOptions.Compiled)
    Private rxColumn = New Regex(regPatternColumn, RegexOptions.Compiled)
    Private rxVFilter = New Regex(regPatternVFilter, RegexOptions.Compiled)
    Private rxCFilter = New Regex(regPatternCFilter, RegexOptions.Compiled)
    Private rxCFValue = New Regex(regPatternCFValue, RegexOptions.Compiled)
    Private rxSelectCondition = New Regex(regPatternSelectCondition, RegexOptions.Compiled)
    Private rxDViewSelectCondition = New Regex(regPatternDViewSelectCondition, RegexOptions.Compiled)
    Private rxDeleteSelectContdition = New Regex(regPatternDeleteSelectContdition, RegexOptions.Compiled)
    Private rxRepository = New Regex(regPatternRepository, RegexOptions.Compiled)
    Private rxProcess = New Regex(regPatternProcess, RegexOptions.Compiled)
    Private rxViewInfo = New Regex(regPatternViewInfo, RegexOptions.Compiled)
    Private rxLookupViewInfo = New Regex(regPatternLookupViewInfo, RegexOptions.Compiled)
    Private rxSquareBracket = New Regex(regPatternSquareBracket, RegexOptions.Compiled)

    'Dictionaly 分別キーワード
    Private Const VIEW As String = "VIEW"
    Private Const COLUMN As String = "COLUMN"
    Private Const COLUMN_LENGTH As String = "COLUMN_LENGTH"

    '[View」テーブル定義名称を格納する。※IDをインデックスとしたストリング名称を格納する。
    'Private hashViewTable


    'TagBlock
    Private intTAG_BLOCK_ID As String
    'Setter/Getter
    Private Function GetTAG_BLOCK_ID() As String
        Return intTAG_BLOCK_ID
    End Function
    Private Function SetTAG_BLOCK_ID(ByVal val As String)
        intTAG_BLOCK_ID = val
    End Function

    'ViewID
    Private intVIEW_ID As String
    'Setter/Getter
    Private Function GetVIEW_ID() As String
        Return intVIEW_ID
    End Function
    Private Function SetVIEW_ID(ByVal val As String)
        intVIEW_ID = val
    End Function

    ''' -----------------------------------------------------------------------------------
    ''' コンストラクタ
    ''' -----------------------------------------------------------------------------------
    Public Sub New()
    End Sub

    Public Sub New(ByVal strPath As String)
        If strPath Is Nothing Then
            Throw New ArgumentNullException(NameOf(strPath))
        End If
    End Sub

    ''' -----------------------------------------------------------------------------------
    ''' 初期化
    ''' -----------------------------------------------------------------------------------
    Public Sub Initialize(ByVal strPath As String)
    End Sub


    Public Sub Conversion(ByVal strPath As String, ByVal outputFilePath As String)

        'ディクショナリー初期化
        dict = New CommonDirectory

        '1.ファイル存在チェック
        If IO.File.Exists(strPath) = False Then
            Exit Sub
        End If

        '出力ファイル名称（タイムスタンプ付）
        Me.OUT_FILE = outputFilePath

        '読込ファイルのタイムスタンプ
        Me.FILE_CREAT_TIME = System.IO.File.GetCreationTime(strPath)
        Dim extention = System.IO.Path.GetExtension(strPath)

        ' -----------------------------------
        ' 書込ストリーマ
        ' -----------------------------------
        Using cWriter = New System.IO.StreamWriter(OUT_FILE, True, System.Text.Encoding.GetEncoding("shift_jis"))
            '「接続情報登録_」で始まる wtj プログラムは構文解析にかけない。  ※定義、処理の構文が存在しないためタイトルのみの表示となる。
            If System.IO.Path.GetExtension(strPath) <> ".wtj" Then
                Me.CONSOLEWRITE("SyntacticAnalysys.Conversion : ", "拡張子が「wtj」でないため処理をキャンセルします。")
                Me.CONSOLEWRITE("SyntacticAnalysys.Conversion : ", strPath)
                Return
            ElseIf OUT_FILE.StartsWith("接続情報登録_") Then
                Me.CONSOLEWRITE("SyntacticAnalysys.Conversion : ", "「接続情報登録_」で始まる wtj プログラムのため処理をキャンセルします。")
                Me.CONSOLEWRITE("SyntacticAnalysys.Conversion : ", strPath)
                Return
            Else
                ' タイトル行の出力
                cWriter.WriteLine(StructRecord.COL_TITLE)
            End If

            ' ---------------------------------
            ' 出力レコードクラスのインスタンス
            ' ---------------------------------
            Me.RECORD = New StructRecord

            '「取込ファイル」 属性情報取得
            Try
                '親ディレクトリ取得　→「格納フォルダ」
                Dim dirInfo As System.IO.DirectoryInfo = System.IO.Directory.GetParent(strPath)
                Me.RECORD.C2_DIR = dirInfo.Name()

                'ファイル名称　→「WTJファイル名」
                Me.RECORD.C3_WTJ_FILE_NAME = System.IO.Path.GetFileName(strPath)

                'ファイル・「更新」タイムスタンプ
                Dim timeStamp As DateTime = System.IO.File.GetLastWriteTime(strPath).ToString
                Me.RECORD.C1_MODIFY_TIME = String.Format("{0:yyyyMMddHHmmss}", timeStamp)

            Catch ex As Exception
                Me.CONSOLEWRITE("Sub Conversion", ex.Message)
            End Try

            Dim intBlockID As UInteger = 0   'タグＩＤ保持用
            Dim intBlockLineCount As UInteger = 0   'タグブロック内行カウンター
            Dim strOutLine As String
            Dim strLine As String
            Dim intStatus As String

            ' --------------------------------------
            ' 読込（パース処理 & サニタイジング）
            ' --------------------------------------
            Using parser As New FileIO.TextFieldParser(strPath, Encoding.GetEncoding("Shift_JIS"))
                ' --------------------------------------
                ' パーサー初期設定
                '     ・カンマ区切りの指定
                '     ・ダブルクオーテーションのエスケープ）  ※フィールドが引用符で囲まれているか
                '     ・フィールドの空白トリム設定
                ' --------------------------------------
                parser.TextFieldType = FieldType.Delimited
                parser.SetDelimiters(vbTab)
                parser.HasFieldsEnclosedInQuotes = False
                parser.TrimWhiteSpace = False
                ' --------------------------------------
                ' ファイル　行読込み
                ' --------------------------------------
                While Not parser.EndOfData
                    ' フィールドを読込
                    Dim row As String()
                    Try
                        row = parser.ReadFields()
                    Catch ex As Exception
                        '「最終行」はException を吐いてしまふ
                        Exit Sub
                    End Try

                    '読み込み番号 (ファイル内通番）
                    intRowCount += 1

                    ' --------------------------------------
                    ' タグキーワードに紐づくＩＤ取得
                    '     ・読込行にタグが検出された場合ブロック内カウンターを初期化する。
                    ' --------------------------------------
                    intStatus = getStatus(row)
                    If (intStatus <> Status.iNone) Then
                        '新しいタグ検知
                        intBlockLineCount = 0
                        intBlockID = intStatus
                    Else
                        intBlockLineCount += 1
                    End If

                    '' -------------------------------------
                    '' 構文解析　※各タグ毎の構文解析を行う
                    '' -------------------------------------
                    strOutLine = Parcer(intBlockID, intBlockLineCount, row)

                    '' -------------------------------------
                    '' 解析結果・ファイル出力
                    '' -------------------------------------
                    If (strOutLine <> Nothing) Then
                        cWriter.WriteLine(strOutLine)

                    End If
                End While

            End Using
        End Using
        'Console.ReadKey()
        'dict.print()
        'MsgBox("END ")

        Me.RECORD = Nothing

    End Sub


#Region "タグキーワードによるメソッド切換"
    ''' -----------------------------------------------------------------------------------
    ''' <summary>
    '''  "タグキーワード"　によりパーサーを切換える    </summary>
    ''' <param name="strLine">
    '''     元になる文字列。</param>
    ''' <returns>
    '''     Me.RECORD.getLine     </returns>
    ''' -----------------------------------------------------------------------------------
    Private Function Parcer(intBlockID As UInteger, intBlockLineCount As UInteger, ByVal strLine() As String) As String

        If (strLine.Length = 0) Then
            Return Nothing
        End If

        ' ------------------------------------------------------
        ' 現在のタグのブロックＩＤによって、構文の処理を変える
        ' ------------------------------------------------------
        Dim retLine = ""
        Select Case GetTAG_BLOCK_ID()
            Case Status.iView
                '■[View]ブロック  ※ビュー定義
                Parcer_View(intBlockID, intBlockLineCount, strLine)
                Return Nothing

            Case Status.iColumn
                '■[Column]ブロック  ※カラム定義
                '※[View]ブロックの次には必ず[Column]ブロックが定義される。
                Parcer_Colum(intBlockID, intBlockLineCount, strLine)
                Return Nothing

            Case Status.iVFilter
                '■[VFilter]ブロック  ビューフィルタ定義
                Parcer_VFilter(intBlockID, intBlockLineCount, strLine)
                Return Nothing

            Case Status.iViewInfo
                '■[ViewInfo]ブロック  ビューフィルタ定義
                'Me.RECORD.C0_SEQ_NUM += 1
                If Parcer_ViewInfo(intBlockID, intBlockLineCount, strLine) Then
                    intTotalLineCount += 1
                    Me.RECORD.C0_SEQ_NUM = intTotalLineCount
                    If DBGFLG Then
                        Return Me.RECORD.getLine(Status.iViewInfo) & ",[ViewInfo]" & intRowCount
                    Else
                        Return Me.RECORD.getLine(Status.iViewInfo)
                    End If
                Else
                    Return Nothing
                End If

            Case Status.iSelectCondition, Status.iDViewSelectCondition, Status.iDeleteSelectContdition
                '■[SelectCondition]ブロック  抽出条件
                If Parcer_SelectCondition(intBlockID, intBlockLineCount, strLine) Then
                    intTotalLineCount += 1
                    Me.RECORD.C0_SEQ_NUM = intTotalLineCount
                    If DBGFLG Then
                        Return Me.RECORD.getLine(Status.iSelectCondition) & ",[SelectCondition]" & intRowCount
                    Else
                        Return Me.RECORD.getLine(Status.iSelectCondition)
                    End If
                Else
                    Return Nothing
                End If

            ' ================ CFilter ================
            Case Status.iCFilter
                '■[CFilter]ブロック 
                'ONSOLEWRITE("Parcer", "■CFilter")
                Parcer_CFilter(intBlockID, intBlockLineCount, strLine)
                Return Nothing

            Case Status.iCFValue
                '■[CFValue]ブロック 
                If Parcer_CFValue(intBlockID, intBlockLineCount, strLine) Then
                    'Return Me.RECORD.getLine(Status.iCFValue)
                    intTotalLineCount += 1
                    Me.RECORD.C0_SEQ_NUM = intTotalLineCount

                    If DBGFLG Then
                        Return Me.RECORD.getLine(Status.iCFValue) & ",[CFValue]" & intRowCount
                    Else
                        Return Me.RECORD.getLine(Status.iCFValue)
                    End If
                Else
                    Return Nothing
                End If

            Case Status.iRepository
                '■[Repository]ブロック  ※Waha バージョンチェック
                'ONSOLEWRITE("Parcer", "■Repository")
                Dim msg = Parcer_Repository(intBlockID, intBlockLineCount, strLine)

                If msg <> Nothing Then
                    MsgBox(msg)
                Else
                    Return Nothing
                End If

            Case Status.iLookupViewInfo
                '■[LookupViewInfo]ブロック  ※
                If Parcer_LookupViewInfo(intBlockID, intBlockLineCount, strLine) Then
                    intTotalLineCount += 1
                    Me.RECORD.C0_SEQ_NUM = intTotalLineCount
                    If DBGFLG Then
                        Return Me.RECORD.getLine(Status.iLookupViewInfo) & ",[LookupViewInfo]" & intRowCount
                    Else
                        Return Me.RECORD.getLine(Status.iLookupViewInfo)
                    End If
                Else
                    Return Nothing
                End If

            Case Status.iProcess
                '■[Process]ブロック  ※前処理ジョブ、後続ジョブ　定義
                If Parcer_Process(intBlockID, intBlockLineCount, strLine) Then
                    intTotalLineCount += 1
                    Me.RECORD.C0_SEQ_NUM = intTotalLineCount
                    If DBGFLG Then
                        Return Me.RECORD.getLine(Status.iProcess) & ",[Process]" & intRowCount
                    Else
                        Return Me.RECORD.getLine(Status.iProcess)
                    End If
                Else
                    Return Nothing
                End If
        End Select

        Return Nothing

    End Function

#End Region


#Region "タグキーワードの検出"
    ''' -----------------------------------------------------------------------------------
    ''' <summary>
    '''  "タグキーワード"をパターンマッチ し、キーワードに対応したＩＤを返す    </summary>
    ''' <param name="row">
    '''     取り出す元になる文字列。</param>
    ''' <returns>
    '''     タグキーワードに対応したＩＤ     </returns>
    ''' -----------------------------------------------------------------------------------
    Private Function getStatus(ByVal row() As String) As UInteger

        If (row.Length = 0) Then
            Return Status.iNone
        End If

        Dim strLine As String = row(0)

        ' --------------------------------------------------------------------
        'タグキーワードのパターンマッチ　※タグが存在した場合、タグIDを返す
        ' --------------------------------------------------------------------
        If rxView.IsMatch(strLine) Then
            SetTAG_BLOCK_ID(Status.iView)
            Return Status.iView
        ElseIf rxColumn.IsMatch(strLine) Then
            SetTAG_BLOCK_ID(Status.iColumn)
            Return Status.iColumn
        ElseIf rxVFilter.IsMatch(strLine) Then
            SetTAG_BLOCK_ID(Status.iVFilter)
            Return Status.iVFilter
        ElseIf rxCFilter.IsMatch(strLine) Then
            'Console.WriteLine("3■    " & strLine)
            SetTAG_BLOCK_ID(Status.iCFilter)
            Return Status.iCFilter
        ElseIf rxCFValue.IsMatch(strLine) Then
            'Console.WriteLine("4■    " & strLine)
            SetTAG_BLOCK_ID(Status.iCFValue)
            Return Status.iCFValue
        ElseIf rxSelectCondition.IsMatch(strLine) Then
            'Console.WriteLine("5■    " & strLine)
            SetTAG_BLOCK_ID(Status.iSelectCondition)
            Return Status.iSelectCondition
        ElseIf rxDViewSelectCondition.IsMatch(strLine) Then
            'Console.WriteLine("6■    " & strLine)
            SetTAG_BLOCK_ID(Status.iDViewSelectCondition)
            Return Status.iDViewSelectCondition
        ElseIf rxDeleteSelectContdition.IsMatch(strLine) Then
            'Console.WriteLine("7■    " & strLine)
            SetTAG_BLOCK_ID(Status.iDeleteSelectContdition)
            Return Status.iDeleteSelectContdition
        ElseIf rxRepository.IsMatch(strLine) Then
            'Console.WriteLine("8■    " & strLine)
            SetTAG_BLOCK_ID(Status.iRepository)
            Return Status.iRepository
        ElseIf rxProcess.IsMatch(strLine) Then
            'Console.WriteLine("9■    " & strLine)
            SetTAG_BLOCK_ID(Status.iProcess)
            Return Status.iProcess
        ElseIf rxViewInfo.IsMatch(strLine) Then
            'Console.WriteLine("10■    " & strLine)
            SetTAG_BLOCK_ID(Status.iViewInfo)
            Return Status.iViewInfo
        ElseIf rxLookupViewInfo.IsMatch(strLine) Then
            'Console.WriteLine("12■    " & strLine)
            SetTAG_BLOCK_ID(Status.iLookupViewInfo)
            Return Status.iLookupViewInfo
        ElseIf rxSquareBracket.IsMatch(strLine) Then
            'Console.WriteLine("13■    " & strLine)
            SetTAG_BLOCK_ID(Status.iSquareBracket)
            Return Status.iSquareBracket
        Else
            Return Status.iNone
        End If

    End Function
#End Region


#Region "Percer : [View]"
    ''' -----------------------------------------------------------------------------------
    ''' <summary>
    '''  ①　[View]タグブロックのパース 
    '''  ※View ブロックからＩ／Ｏテーブル名称を取得し、Viewハッシュにストアする。
    '''  ※タグ以降２行目がパース対象</summary>
    ''' <param name="strLine">
    '''     取り出す元になる文字列。</param>
    ''' -----------------------------------------------------------------------------------
    Private Sub Parcer_View(intBlockID As UInteger, intBlockLineCount As UInteger, ByVal strLine() As String)

        If (strLine.Length = 0) Then
            Return
        End If

        Try
            Dim columnsList As String()
            If intBlockLineCount = 2 Then
                ' カンマ区切りで分割して配列に格納する
                columnsList = strLine

                'ViewハッシュにテーブルＩＤとテーブル名称を登録
                If columnsList.Length > 7 Then
                    Dim viewID As String = columnsList(7)    'View ID
                    Dim viewName As String = columnsList(8)  'View 名称
                    SetVIEW_ID(viewID)

                    'View ID をハッシュ登録
                    dict.SetAttr(VIEW, GetVIEW_ID(), viewName)

                End If
            End If
        Catch ex As System.AggregateException
            MessageBox.Show(ex.StackTrace, "SyntacticAnalysis.Parcer_View")
        End Try
    End Sub
#End Region


#Region "Percer : [View]-[Colum]"
    ''' -----------------------------------------------------------------------------------
    ''' <summary>
    '''  ②　[Colum]タグブロックのパース 
    '''  Colum ブロックから属性名称を取得し、Columハッシュにストアする。
    '''  タグ以降２行目がパース対象</summary>
    ''' <param name="strLine">
    '''     取り出す元になる文字列。</param>
    ''' -----------------------------------------------------------------------------------
    Private Sub Parcer_Colum(intBlockID As UInteger, intBlockLineCount As UInteger, ByVal strLine() As String)

        If (strLine.Length = 0) Then
            Return
        End If

        Dim columnsList As String()
        If intBlockLineCount >= 2 Then
            ' カンマ区切りで分割して配列に格納する
            columnsList = strLine

            'ViewハッシュにテーブルＩＤとテーブル名称を登録
            If columnsList.Length > 6 Then
                Dim columnName As String = columnsList(5)   '属性名称
                Dim columnLength As String = columnsList(6) '属性長
                Dim columnID As String = columnsList(10)    '属性ID
                Dim key As String = GetVIEW_ID() & "_" & columnID

                '属性名称、属性長をハッシュにストア
                Me.dict.SetAttr(COLUMN, GetVIEW_ID, columnID, columnName)
                Me.dict.SetAttr(COLUMN_LENGTH, GetVIEW_ID, columnID, columnLength)

            End If
        End If
    End Sub
#End Region


#Region "Percer : [VFilter]"
    ''' -----------------------------------------------------------------------------------
    ''' <summary>
    '''  [View]タグブロックのパース 
    '''  
    '''  Colum ブロックから属性名称を取得し、Columハッシュにストアする。
    '''  タグ以降２行目がパース対象</summary>
    ''' <param name="strLine">
    '''     取り出す元になる文字列。</param>
    ''' -----------------------------------------------------------------------------------
    Private Sub Parcer_VFilter(intBlockID As UInteger, intBlockLineCount As UInteger, ByVal strLine() As String)

        If (strLine.Length = 0) Then
            Return
        End If

        Try
            Dim columns As String() = strLine

            If intBlockLineCount = 1 Then
                ' [1]タイトル行
                'ビューフィルタ構造体 初期化
                recordViewFilter = New STRUCT_VIEW_FILTER With {
                        .AtrributeList = columns
                    }
            ElseIf intBlockLineCount >= 2 Then
                ' [2]フィルター定義｛フィルター種別、フィルター名称｝
                ' カンマ区切りで分割して配列に格納する
                '配列中キーワードにマッチしたインデックス
                Dim indexKind As Integer = getKeysindex(recordViewFilter.AtrributeList, "Kind")
                Dim indexId As Integer = getKeysindex(recordViewFilter.AtrributeList, "ID")
                Dim indexrName As String = getKeysindex(recordViewFilter.AtrributeList, "Name")

                'レコードオブジェクトに設定
                Me.RECORD.C4_VIEW_FILTER_KBN = columns(indexKind)
                Me.RECORD.C5_VIEW_FILTER_KBN_NAME = Me.dict.GetAttr(VIEW_FILTER_CLASS, Me.RECORD.C4_VIEW_FILTER_KBN)

                'ビューフィルタの名称は、StructRecore中にて設定するためここでは不定
                Me.RECORD.C6_VIEW_FILTER_NAME = columns(indexrName)
            End If

        Catch ex As System.Exception
            MessageBox.Show(ex.StackTrace, "xxxxxxxxxxxx")
        End Try
    End Sub
#End Region


#Region "Percer : [VFilter]-[VewInfo]"
    ''' -----------------------------------------------------------------------------------
    ''' <summary>
    '''  [VewInfo]タグブロックのパース 
    '''  Colum ブロックから属性名称を取得し、Columハッシュにストアする。
    '''  タグ以降２行目がパース対象</summary>
    ''' <param name="strLine">
    '''     取り出す元になる文字列。</param>
    ''' -----------------------------------------------------------------------------------
    Private Function Parcer_ViewInfo(intBlockID As UInteger, intBlockLineCount As UInteger, ByVal strLine() As String) As Boolean
        If (strLine.Length = 0) Then
            Return False
        End If

        Dim arryColumns As String()
        'ブロック内行カウンターが２行目以上　※１行目はカラムタイトル
        If intBlockLineCount > 1 Then
            ' カンマ区切りで分割して配列に格納する
            arryColumns = strLine

            Dim strViewKind = ""   '0:入力ファイル、1:分割ファイル
            Dim strViewId = ""     '前処理[0]、後処理[1]
            Dim strInputViewId As String = ""       '入力ビューID
            Dim strInputView As String = ""         '入力ビュー名称
            Dim strOutputViewId As String = ""      '出力ビューID
            Dim strOutputView As String = ""        '出力ビュー名称
            strViewKind = arryColumns(0).Trim   '0:入力ファイル、1:分割ファイル
            strViewId = arryColumns(1).Trim     '前処理[0]、後処理[1]

            If (strViewKind.StartsWith("0")) Then
                '①入力情報
                strInputViewId = strViewId
                strInputView = CStr(dict.GetAttr(VIEW, strViewId))
                strOutputViewId = "-"
                strOutputView = "-"
                '移送元ビューID/名称
                Me.RECORD.C12_ORG_VIEW_ID = strViewId
                Me.RECORD.C13_ORG_VIEW_NAME = strInputView
                '移送先ビューID/名称
                Me.RECORD.C7_DIST_VIEW_ID = "-"
                Me.RECORD.C8_DIST_VIEW_NAME = "-"
                Return True
            Else
                '②出力情報
                strOutputViewId = strViewId
                '移送先ビューID/名称
                Me.RECORD.C7_DIST_VIEW_ID = strViewId
                Me.RECORD.C8_DIST_VIEW_NAME = CStr(dict.GetAttr(VIEW, strViewId))
                Return True
            End If
        End If
        Return False
    End Function
#End Region


#Region "Percer : [VFilter]-[SelectCondition], [DViewSelectCondition], [DeleteSelectCondition]"
    ''' <summary>
    '''  [SelectCondition]タグブロックのパース 
    '''   ブロックから抽出条件を取得する。
    '''  タグ以降２行目がパース対象</summary>
    ''' <param name="strLine">
    '''     取り出す元になる文字列。</param>
    Private Function Parcer_SelectCondition(intBlockID As UInteger, intBlockLineCount As UInteger, ByVal strLine() As String) As Boolean
        If (strLine.Length = 0) Then
            Return False
        End If

        Dim arryColumns As String()

        'ブロック内行カウンターが 2行目以上処理対象　※１行目はカラムタイトル
        If intBlockLineCount >= 2 Then
            ' カンマ区切りで分割して配列に格納する
            arryColumns = strLine

            Dim strSelectConditionColumnID = ""

            '移送元　属性情報の取得
            '参照ビューＩＤ
            Dim strSelectConditionViewID = arryColumns(0).Trim
            '設定値
            Dim strSelectConditionValue = arryColumns(2).Trim
            '演算子
            Dim strSelectConditionOperator = arryColumns(3).Trim

            Try
                strSelectConditionColumnID = arryColumns(10).Trim
            Catch ex As Exception
                MessageBox.Show(ex.Message)
                strSelectConditionColumnID = ""
            End Try

            '属性名称を取得
            Dim key As String = strSelectConditionViewID & "_" & strSelectConditionColumnID
            Dim val As String
            Try
                val = dict.GetAttr(COLUMN, strSelectConditionViewID, strSelectConditionColumnID)
                If val = Nothing Then
                    Return False
                End If

                Me.RECORD.C12_ORG_VIEW_ID = CStr(strSelectConditionViewID)
                Me.RECORD.C13_ORG_VIEW_NAME = CStr(dict.GetAttr(VIEW, strSelectConditionViewID))
                Me.RECORD.C16_ORG_COL_NAME = CStr(val)
                Me.RECORD.C23_SELECT_TERM = CStr(val)
                Me.RECORD.C22_SELECT_OPERATER = CStr(dict.GetAttr(OPE, strSelectConditionOperator))
                Me.RECORD.C21_SELECT_CONDITION = strSelectConditionValue

            Catch ex As Exception
                MessageBox.Show(ex.Message)
                Return False
            End Try

            Return True
        End If

        Return False

    End Function
#End Region


#Region "Percer : [CFilter] "
    ''' <summary>
    '''  [CFilter]タグブロックのパース 
    '''   移送先・属性情報
    '''  </summary>
    ''' <param name="strLine">
    '''     取り出す元になる文字列。</param>
    Private Function Parcer_CFilter(intBlockID As UInteger, intBlockLineCount As UInteger, ByVal strLine() As String) As Boolean
        If (strLine.Length = 0) Then
            Return False
        End If

        'ブロック内行カウンターが２行目以上　※１行目はカラムタイトル
        Dim arryColumns As String()
        If intBlockLineCount >= 2 Then
            ' カンマ区割
            arryColumns = strLine
            '移送先ビューＩＤ
            Dim strDistViewId = arryColumns(0).Trim
            '属性項目
            Dim strDistColId = arryColumns(1).Trim

            '属性名称を取得
            Dim val As String
            Try
                Dim key As String = strDistViewId & "_" & strDistColId
                val = CStr(dict.GetAttr(COLUMN, strDistViewId, strDistColId))
                If val = Nothing Then
                    Return False
                End If
            Catch ex As Exception
                Return False
            End Try

            Me.RECORD.C12_ORG_VIEW_ID = strDistViewId
            Me.RECORD.C8_DIST_VIEW_NAME = CStr(dict.GetAttr(VIEW, strDistViewId))
            Me.RECORD.C9_DIST_COL_ID = strDistColId
            Me.RECORD.C11_DIST_COL_NAME = CStr(dict.GetAttr(COLUMN, strDistViewId, strDistColId))
            Me.RECORD.C10_DIST_COL_LEN = CStr(dict.GetAttr(COLUMN_LENGTH, strDistViewId, strDistColId))

            Return True

        End If

        Return False

    End Function
#End Region

#Region "Percer : [CFilter]-[CFValue] "
    ''' <summary>
    '''  [CFValue]タグブロックのパース 
    '''   移送先・属性情報
    '''  </summary>
    ''' <param name="strLine">
    '''     取り出す元になる文字列。</param>
    Private Function Parcer_CFValue(intBlockID As UInteger, intBlockLineCount As UInteger, ByVal strLine() As String) As Boolean
        If (strLine.Length = 0) Then
            Return False
        End If

        Dim arryColumns As String()
        'ブロック内行カウンターが２行目以上　※１行目はカラムタイトル
        If intBlockLineCount >= 2 Then

            arryColumns = strLine

            ' 移送元・ビューID
            Dim c0_ViewID = arryColumns(0).Trim()
            '' 移送元・型
            Dim c1_ValueType = arryColumns(1).Trim()
            ' 移送元・移送値
            Dim c2_Value = arryColumns(2).Trim()
            Dim c3_SetValueType = arryColumns(3).Trim()
            ' 関数タイプ
            Dim c4_SFType = arryColumns(4).Trim()

            ' 実値型
            Dim c5_RealValueType = arryColumns(5).Trim()
            Dim c6_ObjType = arryColumns(6).Trim()
            Dim c7_Name = arryColumns(7).Trim()
            Dim c8_LookupInfoID = arryColumns(8).Trim()
            ' 移送元・カラムID
            Dim c9_LinkColID = arryColumns(9).Trim()
            ' ルートフラグ （直接移送）
            Dim c10_IsRoot = arryColumns(10).Trim()
            Dim c11_ID = arryColumns(11).Trim()
            Dim c12_Comment = arryColumns(12).Trim()

            ' パラメータ （直接移送）
            Dim c13_ChildIDs
            If (arryColumns.Length >= 14) Then
                c13_ChildIDs = arryColumns(13).Trim()
            Else
                c13_ChildIDs = ""
            End If

            '移送元・属性情報
            Try
                Dim functionNum As Integer
                Try
                    functionNum = Integer.Parse(c4_SFType)
                Catch ex As Exception
                    functionNum = 0
                End Try

                Me.RECORD.C19_FUNCTION_TYPE = CStr(dict.GetAttr(FUNCT, c4_SFType))

                '移送元・ビューID
                Me.RECORD.C12_ORG_VIEW_ID = CStr(c0_ViewID)
                Me.RECORD.C18_TRANCE_VALUE = CStr(c2_Value)
                '移送元・ビューテーブル名称
                Me.RECORD.C13_ORG_VIEW_NAME = CStr(dict.GetAttr(VIEW, c0_ViewID))
                '移送元・カラム名称
                Me.RECORD.C14_ORG_COL_ID = CStr(c9_LinkColID)
                '移送元・カラム長
                Me.RECORD.C15_ORG_COL_LEN = dict.GetAttr(COLUMN_LENGTH, c0_ViewID, c9_LinkColID)

                '移送元・カラム名称
                Me.RECORD.C16_ORG_COL_NAME = CStr(dict.GetAttr(COLUMN, c0_ViewID, c9_LinkColID))
                Me.RECORD.C17_TRANCE_TYPE = CStr(c5_RealValueType)
                Me.RECORD.C19_FUNCTION_TYPE = CStr(c4_SFType)

                '直送フラグ
                ' IsRoot=1 かつ ChildsIds=0 の場合、"直接移送" とみなす
                If c10_IsRoot = "1" And c13_ChildIDs = "0" Then
                    Me.RECORD.C20_DIRECT_FLG = "1"
                Else
                    Me.RECORD.C20_DIRECT_FLG = ""
                End If

                Dim functionName As String
                Dim operatorName As String
                Dim value As String
                If c0_ViewID = "0" And c9_LinkColID = "0" Then
                    '①Waha関数設定
                    If functionNum > 0 Then
                        Me.RECORD.C14_ORG_COL_ID = "0"
                        Me.RECORD.C15_ORG_COL_LEN = "0"

                        functionName = dict.GetAttr(FUNCT, c4_SFType)
                        'If functionName.Length > 0 Then
                        If functionName <> Nothing Then
                            Me.RECORD.C16_ORG_COL_NAME = "■Function[" & functionName & "]"
                        Else
                            '関数がDictionaryに登録されていない場合 , 関数IDを出力
                            Me.RECORD.C16_ORG_COL_NAME = "■Function[" & functionNum & "]"
                        End If

                    ElseIf c5_RealValueType <> "0" Then
                        '②移送元Value : 　サニタイジング（エスケープ) 処理
                        ' ※サニタイジング処理は、TextFieldParcer で読込みしているため考慮不要
                        Me.RECORD.C16_ORG_COL_NAME = "□Value[" & c2_Value & "]"
                        Me.RECORD.C18_TRANCE_VALUE = c2_Value

                    ElseIf c1_ValueType = "6" And c3_SetValueType = "101" And c6_ObjType = "5" Then
                        'TODO
                        Me.RECORD.C16_ORG_COL_NAME = ""
                        'オペレータ取得
                        operatorName = dict.GetAttr(OPE, CStr(c2_Value))
                        Me.RECORD.C16_ORG_COL_NAME = "□Ope[" & operatorName & "]"
                        Me.RECORD.C17_TRANCE_TYPE = ""

                    ElseIf c1_ValueType = "1" And c3_SetValueType = "8" And c6_ObjType = "3" Then
                        Me.RECORD.C23_SELECT_TERM = ""
                        Me.RECORD.C16_ORG_COL_NAME = "□Value[Null]"
                    Else
                        Me.RECORD.C23_SELECT_TERM = ""
                        Me.RECORD.C16_ORG_COL_NAME = "□未設定"
                    End If

                Else
                End If

                Return True
            Catch ex As Exception
                CONSOLEWRITE("Parcer_CFValue", ex.ToString())
                Return False
            End Try
            'Return True
        End If

        Return False

    End Function
#End Region


#Region "Percer : [LookupViewInfo]"
    ''' <summary>
    ''' [LookupViewInfo] ブロック処理
    ''' 標準入力ファイル　１、出力ファイル　１
    ''' 分割：入力ファイル　１、出力ファイル　複数
    '''   移送先・属性情報
    '''  </summary>
    ''' <param name="strLine">
    '''     取り出す元になる文字列。</param>
    Private Function Parcer_LookupViewInfo(intBlockID As UInteger, intBlockLineCount As UInteger, ByVal strLine() As String) As Boolean
        If (strLine.Length = 0) Then
            Return False
        End If

        'ブロック内行カウンターが２行目以上　※１行目はカラムタイトル
        Dim arryColumns As String()
        If intBlockLineCount >= 2 Then
            Try
                ' カンマ区割
                arryColumns = strLine
                ' 移送元・ビューID
                Dim c0_ViewID = arryColumns(0).Trim()
                '  参照ビューID
                Me.RECORD.C12_ORG_VIEW_ID = CStr(c0_ViewID)
                '  参照ビュー名称
                Me.RECORD.C13_ORG_VIEW_NAME = dict.GetAttr(VIEW, c0_ViewID)
                Return True

            Catch ex As Exception
                CONSOLEWRITE("Parcer_LookupViewInfo", ex.Message())
                Return False
            End Try
        End If

        Return False

    End Function
#End Region


#Region "Percer : [Repository]"
    ''' <summary>
    ''' [Repository] ブロック処理
    ''' 対象ヴァージョンは Waha 2.4。これ以外のバージョンの場合、メッセージボックスを出して終了。
    '''  </summary>
    ''' <param name="strLine">
    '''     取り出す元になる文字列。</param>
    Private Function Parcer_Repository(intBlockID As UInteger, intBlockLineCount As UInteger, ByVal strLine() As String) As String
        If (strLine.Length = 0) Then
            Return False
        End If

        Dim arryColumns As String()
        'ブロック内行カウンターが２行目以上　※１行目はカラムタイトル
        If intBlockLineCount = 2 Then
            Try
                ' カンマ区割
                arryColumns = strLine
                'バージョンの取得
                Dim repositoryVersion = arryColumns(1).Trim()
                Dim msg As String = ""
                If repositoryVersion.StartsWith("2") Then
                    'OK
                Else
                    msg = "[WahaVersionCheckError] : WTJファイルは、Waha [" & repositoryVersion & " ] です。バージョン対象外のため解析ができません。"
                End If

                Return msg

            Catch ex As Exception
                CONSOLEWRITE("Parcer_LookupViewInfo", ex.Message())
                Return Nothing
            End Try

        Else
            Return Nothing
        End If

    End Function
#End Region


#Region "Percer : [Process]"
    ''' <summary>
    ''' [Process] ブロック処理
    ''' 標準入力ファイル　１、出力ファイル　１
    ''' 分割：入力ファイル　１、出力ファイル　複数
    '''   移送先・属性情報
    '''  </summary>
    ''' <param name="strLine">
    '''     取り出す元になる文字列。</param>
    Private Function Parcer_Process(intBlockID As UInteger, intBlockLineCount As UInteger, ByVal strLine() As String) As Boolean
        If (strLine.Length = 0) Then
            Return False
        End If

        'ブロック内行カウンターが２行目以上　※１行目はカラムタイトル
        Dim arryColumns As String()
        If intBlockLineCount > 1 Then
            Try
                'カンマ区割
                arryColumns = strLine

                'ディレクトリ
                Dim workDir = arryColumns(0).Trim()
                '前処理[0], 後処理[1]
                Dim dicision = arryColumns(11).Trim()
                'コマンド
                Dim command = arryColumns(12).Trim()

                'レコードオブジェクトに設定
                'Me.RECORD.C23_SELECT_TERM = dicision
                Me.RECORD.C24_WORK_DIR = workDir
                Me.RECORD.C25_JOB = dicision
                Me.RECORD.C26_COMMAND = command

                Return True
            Catch ex As Exception
                CONSOLEWRITE("Parcer_Process", ex.ToString())
                Return False
            End Try

        Else
            Return False
        End If

    End Function
#End Region


#Region "Util"
    ''' -----------------------------------------------------------------------------------
    ''' <summary>
    '''  配列中から、Key に一致するIndex を返す。
    '''  </summary>
    ''' <param name="key">
    '''     取り出す元になる文字列。</param>
    ''' -----------------------------------------------------------------------------------
    Private Function getKeysindex(keyList As String(), key As String)

        If (key = Nothing) Then
            Return -1
        End If
        key = key.Trim

        Dim foo As String
        For i As Integer = 0 To keyList.Length - 1
            foo = keyList(i).Trim
            If String.Equals(foo, key) Then
                'マッチしたインデックスを返す
                Return i
            End If
        Next

        '全て不一致
        Return -1
    End Function


    Private Sub CONSOLEWRITE(foo As String, var As String)
        Console.WriteLine(foo & " : " & var)
    End Sub

#End Region

End Class
