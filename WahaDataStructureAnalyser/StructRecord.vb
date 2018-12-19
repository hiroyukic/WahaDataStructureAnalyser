' -----------------------------------------------------------------------------------------
' 《出力フォーマット (20130604)》
'  1 シーケンス番号	　数値文字列	15		許可	※出力シーケンス(20130604 新規追加)
'  2 タイムスタンプ	　数値文字列	15		許可	※20130418 新規追加
'  3 WTJファイル名	　　　　　　文字	128		許可	WTJファイル名称
'  4 ビューフィルタ種別	　　数値	2	0	許可	0：標準, 1：結合, 2：統合, 4：, 5：参照
'  5 ビューフィルタ種別名称	文字	8		許可
'  6 ビューフィルタ名称	　　文字	99		許可	ViewFilter名称
'  7 移送先ビューID	　数値文字列	3	0	許可	移送先・ビューID
'  8 移送先ビューテーブル名称	文字	99		許可
'  9 移送先カラムID	　数値文字列	4	0	許可	移送先・カラムID
' 10 移送先カラム長	　数値文字列	4	0	許可
' 11 移送先カラム名称	　　　　文字	99		許可	移送先・カラム名称
' 12 移送元ビューID	　数値文字列	3	0	許可	移送元・ビューID
' 13 移送元ビューテーブル名称	文字	99		許可
' 14 移送元カラムID	　数値文字列	5	0	許可	移送元・カラムID
' 15 移送元カラム長	　数値文字列	4	0	許可
' 16 移送元カラム名称	　　　　文字	256		許可	移送元・カラム名称
' 17 移送値型	　　　　　数値文字列	3	0	許可	Valタイプフラグ
' 18 移送値	　　　　　　　　文字	256		許可	実値(Value)
' 19 関数タイプ	　　　数値文字列	4	0	許可	関数ID
' 20 直接移送フラグ	　数値文字列	1	0	許可	１：直接移送
' 21 抽出条件（値）	　　　　文字	32		許可
' 22 抽出条件（演算子）	　　文字	8		許可
' 23 抽出条件（項目）	　　　　文字	64		許可
' -----------------------------------------------------------------------------------------

Public Class StructRecord

    'Private ENUMTAG = New EnumTag()

    'ビューフィルタ種別名称・ディクショナリー
    Private dicVIEW_FILTER_NAME = New Dictionary(Of String, String)

    'オペレータ演算子・ディクショナリー
    Private DIC_OPERATOR_NAME = New Dictionary(Of String, String)

    Private OUT_TERM(26) As String

    ' タグ・キーワード
    Public Enum Tag
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

    ' タイトル行 _
    Public Const COL_TITLE As String = "0SeqNo," _
            & "1_タイムスタンプ," _
            & "2_格納フォルダ（業務名）," _
            & "3_WTJファイル名," _
            & "4_ビューフィルタ種別," _
            & "5_ビューフィルタ種別名称," _
            & "6_ビューフィルタ名称," _
            & "7_移送先ビューID," _
            & "8_移送先ビューテーブル名称," _
            & "9_移送先カラムID," _
            & "10_移送先カラム長," _
            & "11_移送先カラム名称," _
            & "12_移送元ビューID," _
            & "13_移送元ビューテーブル名称," _
            & "14_移送元カラムID," _
            & "15_移送元カラム長," _
            & "16_移送元カラム名称," _
            & "17_移送値型," _
            & "18_移送値," _
            & "19_関数タイプ," _
            & "20_直送フラグ," _
            & "21_抽出条件（条件値）," _
            & "22_抽出条件（比較演算子）," _
            & "23_抽出条件（項目）," _
            & "24_WorkDir," _
            & "25_処理（0:前／1:後)," _
            & "26_Command"


    Public Enum columns
        c0_SEQ_NUM
        c1_MODIFY_TIME
        c2_DIR
        c3_WTJ_FILE_NAME
        c4_VIEW_FILTER_KBN
        c5_VIEW_FILTER_KBN_NAME
        c6_VIEW_FILTER_NAME
        c7_DIST_VIEW_ID
        c8_DIST_VIEW_NAME
        c9_DIST_COL_ID
        c10_DIST_COL_LEN
        c11_DIST_COL_NAME
        c12_ORG_VIEW_ID
        c13_ORG_VIEW_NAME
        c14_ORG_COL_ID
        c15_ORG_COL_LEN
        c16_ORG_COL_NAME
        c17_TRANCE_TYPE
        c18_TRANCE_VALUE
        c19_FUNCTION_TYPE
        c20_DIRECT_FLG
        c21_SELECT_CONDITION
        c22_SELECT_OPERATER
        c23_SELECT_TERM
        c24_WORK_DIR
        c25_JOB
        c26_COMMAND
    End Enum


    ' 出力フォーマット用属格納エリア
    Private _C0_SEQ_NUM As UInteger = 0
    Private _C1_MODIFY_TIME As String
    Private _C2_DIR As String
    Private _C3_WTJ_FILE_NAME As String
    Private _C4_VIEW_FILTER_KBN As String
    Private _C5_VIEW_FILTER_KBN_NAME As String
    Private _C6_VIEW_FILTER_NAME As String
    Private _C7_DIST_VIEW_ID As String
    Private _C8_DIST_VIEW_NAME As String
    Private _C9_DIST_COL_ID As String
    Private _C10_DIST_COL_LEN As String
    Private _C11_DIST_COL_NAME As String
    Private _C12_ORG_VIEW_ID As String
    Private _C13_ORG_VIEW_NAME As String
    Private _C14_ORG_COL_ID As String
    Private _C15_ORG_COL_LEN As String
    Private _C16_ORG_COL_NAME As String
    Private _C17_TRANCE_TYPE As String
    Private _C18_TRANCE_VALUE As String
    Private _C19_FUNCTION_TYPE As String
    Private _C20_DIRECT_FLG As String
    Private _C21_SELECT_CONDITION As String
    Private _C22_SELECT_OPERATER As String
    Private _C23_SELECT_TERM As String
    Private _C24_WORK_DIR As String
    Private _C25_JOB As String
    Private _C26_COMMAND As String

    Public Property C0_SEQ_NUM As UInteger
        Get
            Return _C0_SEQ_NUM
        End Get
        Set(value As UInteger)
            _C0_SEQ_NUM = value
        End Set
    End Property

    Public Property C1_MODIFY_TIME As String
        Get
            Return _C1_MODIFY_TIME
        End Get
        Set(value As String)
            _C1_MODIFY_TIME = value
        End Set
    End Property

    Public Property C2_DIR As String
        Get
            Return _C2_DIR
        End Get
        Set(value As String)
            _C2_DIR = value
        End Set
    End Property

    Public Property C3_WTJ_FILE_NAME As String
        Get
            Return _C3_WTJ_FILE_NAME
        End Get
        Set(value As String)
            _C3_WTJ_FILE_NAME = value
        End Set
    End Property

    Public Property C4_VIEW_FILTER_KBN As String
        Get
            Return _C4_VIEW_FILTER_KBN
        End Get
        Set(value As String)
            _C4_VIEW_FILTER_KBN = value
        End Set
    End Property

    Public Property C5_VIEW_FILTER_KBN_NAME As String
        Get
            Return _C5_VIEW_FILTER_KBN_NAME
        End Get
        Set(value As String)
            _C5_VIEW_FILTER_KBN_NAME = value
        End Set
    End Property

    Public Property C6_VIEW_FILTER_NAME As String
        Get
            Return _C6_VIEW_FILTER_NAME
        End Get
        Set(value As String)
            _C6_VIEW_FILTER_NAME = value
        End Set
    End Property

    Public Property C7_DIST_VIEW_ID As String
        Get
            Return _C7_DIST_VIEW_ID
        End Get
        Set(value As String)
            _C7_DIST_VIEW_ID = value
        End Set
    End Property

    Public Property C8_DIST_VIEW_NAME As String
        Get
            Return _C8_DIST_VIEW_NAME
        End Get
        Set(value As String)
            _C8_DIST_VIEW_NAME = value
        End Set
    End Property

    Public Property C9_DIST_COL_ID As String
        Get
            Return _C9_DIST_COL_ID
        End Get
        Set(value As String)
            _C9_DIST_COL_ID = value
        End Set
    End Property

    Public Property C10_DIST_COL_LEN As String
        Get
            Return _C10_DIST_COL_LEN
        End Get
        Set(value As String)
            _C10_DIST_COL_LEN = value
        End Set
    End Property

    Public Property C11_DIST_COL_NAME As String
        Get
            Return _C11_DIST_COL_NAME
        End Get
        Set(value As String)
            _C11_DIST_COL_NAME = value
        End Set
    End Property

    Public Property C12_ORG_VIEW_ID As String
        Get
            Return _C12_ORG_VIEW_ID
        End Get
        Set(value As String)
            _C12_ORG_VIEW_ID = value
        End Set
    End Property

    Public Property C13_ORG_VIEW_NAME As String
        Get
            Return _C13_ORG_VIEW_NAME
        End Get
        Set(value As String)
            _C13_ORG_VIEW_NAME = value
        End Set
    End Property

    Public Property C14_ORG_COL_ID As String
        Get
            Return _C14_ORG_COL_ID
        End Get
        Set(value As String)
            _C14_ORG_COL_ID = value
        End Set
    End Property

    Public Property C15_ORG_COL_LEN As String
        Get
            Return _C15_ORG_COL_LEN
        End Get
        Set(value As String)
            _C15_ORG_COL_LEN = value
        End Set
    End Property

    Public Property C16_ORG_COL_NAME As String
        Get
            Return _C16_ORG_COL_NAME
        End Get
        Set(value As String)
            _C16_ORG_COL_NAME = value
        End Set
    End Property

    Public Property C17_TRANCE_TYPE As String
        Get
            Return _C17_TRANCE_TYPE
        End Get
        Set(value As String)
            _C17_TRANCE_TYPE = value
        End Set
    End Property

    Public Property C18_TRANCE_VALUE As String
        Get
            Return _C18_TRANCE_VALUE
        End Get
        Set(value As String)
            _C18_TRANCE_VALUE = value
        End Set
    End Property

    Public Property C19_FUNCTION_TYPE As String
        Get
            Return _C19_FUNCTION_TYPE
        End Get
        Set(value As String)
            _C19_FUNCTION_TYPE = value
        End Set
    End Property

    Public Property C20_DIRECT_FLG As String
        Get
            Return _C20_DIRECT_FLG
        End Get
        Set(value As String)
            _C20_DIRECT_FLG = value
        End Set
    End Property

    Public Property C21_SELECT_CONDITION As String
        Get
            Return _C21_SELECT_CONDITION
        End Get
        Set(value As String)
            _C21_SELECT_CONDITION = value
        End Set
    End Property

    Public Property C22_SELECT_OPERATER As String
        Get
            Return _C22_SELECT_OPERATER
        End Get
        Set(value As String)
            _C22_SELECT_OPERATER = value
        End Set
    End Property

    Public Property C23_SELECT_TERM As String
        Get
            Return _C23_SELECT_TERM
        End Get
        Set(value As String)
            _C23_SELECT_TERM = value
        End Set
    End Property

    Public Property C24_WORK_DIR As String
        Get
            Return _C24_WORK_DIR
        End Get
        Set(value As String)
            _C24_WORK_DIR = value
        End Set
    End Property

    Public Property C25_JOB As String
        Get
            Return _C25_JOB
        End Get
        Set(value As String)
            _C25_JOB = value
        End Set
    End Property

    Public Property C26_COMMAND As String
        Get
            Return _C26_COMMAND
        End Get
        Set(value As String)
            _C26_COMMAND = value
        End Set
    End Property


    Public Function getLine(tagStatus As UInteger) As String
        Dim strOutLine As String = ""
        '現在のタグのブロックＩＤによって、構文の処理を変える
        Select Case tagStatus

            'Case Status.iView
            ' [View]タグはビュー定義のみ　出力は無し
            'Case Status.iColumn
            ' [Column]タグはカラム定義のみ　出力は無し
            'Case Me.Tag.iVFilter
            ' [VFilter]タグはビュー定義のみ　出力は無し

            Case Tag.iVFilter
                'ビューフィルタ1件の情報
                '[VFilter]ブロック  ビューフィルタ定義
                OUT_TERM(columns.c0_SEQ_NUM) = Me.C0_SEQ_NUM
                OUT_TERM(columns.c1_MODIFY_TIME) = Me.C20_DIRECT_FLG
                OUT_TERM(columns.c2_DIR) = Me.C2_DIR
                OUT_TERM(columns.c3_WTJ_FILE_NAME) = Me.C3_WTJ_FILE_NAME
                'ビューフィルタ区分
                OUT_TERM(columns.c4_VIEW_FILTER_KBN) = Me.C4_VIEW_FILTER_KBN
                'ビューフィルタ区分名称
                OUT_TERM(columns.c5_VIEW_FILTER_KBN_NAME) = Me.C5_VIEW_FILTER_KBN_NAME
                OUT_TERM(columns.c6_VIEW_FILTER_NAME) = Me.C6_VIEW_FILTER_NAME
                OUT_TERM(columns.c7_DIST_VIEW_ID) = Me.C7_DIST_VIEW_ID
                OUT_TERM(columns.c8_DIST_VIEW_NAME) = Me.C8_DIST_VIEW_NAME
                OUT_TERM(columns.c9_DIST_COL_ID) = ""
                OUT_TERM(columns.c10_DIST_COL_LEN) = ""
                OUT_TERM(columns.c11_DIST_COL_NAME) = ""
                OUT_TERM(columns.c12_ORG_VIEW_ID) = ""
                OUT_TERM(columns.c13_ORG_VIEW_NAME) = ""
                OUT_TERM(columns.c14_ORG_COL_ID) = ""
                OUT_TERM(columns.c15_ORG_COL_LEN) = ""
                OUT_TERM(columns.c16_ORG_COL_NAME) = ""
                OUT_TERM(columns.c17_TRANCE_TYPE) = ""
                OUT_TERM(columns.c18_TRANCE_VALUE) = ""
                OUT_TERM(columns.c19_FUNCTION_TYPE) = ""
                OUT_TERM(columns.c20_DIRECT_FLG) = ""
                OUT_TERM(columns.c21_SELECT_CONDITION) = ""
                OUT_TERM(columns.c22_SELECT_OPERATER) = ""
                OUT_TERM(columns.c23_SELECT_TERM) = ""
                OUT_TERM(columns.c24_WORK_DIR) = ""
                OUT_TERM(columns.c25_JOB) = ""
                OUT_TERM(columns.c26_COMMAND) = ""
                '出力行・整形
                strOutLine = getFormatLine(OUT_TERM)
                Return strOutLine

            Case Tag.iViewInfo
                '[ViewInfo]ブロック  ビューフィルタ定義
                OUT_TERM(columns.c0_SEQ_NUM) = Me.C0_SEQ_NUM
                OUT_TERM(columns.c1_MODIFY_TIME) = Me.C1_MODIFY_TIME
                OUT_TERM(columns.c2_DIR) = Me.C2_DIR
                OUT_TERM(columns.c3_WTJ_FILE_NAME) = Me.C3_WTJ_FILE_NAME
                'ビューフィルタ区分
                OUT_TERM(columns.c4_VIEW_FILTER_KBN) = Me.C4_VIEW_FILTER_KBN
                'ビューフィルタ区分名称
                OUT_TERM(columns.c5_VIEW_FILTER_KBN_NAME) = Me.C5_VIEW_FILTER_KBN_NAME & "ViewInfo"
                OUT_TERM(columns.c6_VIEW_FILTER_NAME) = Me.C6_VIEW_FILTER_NAME
                OUT_TERM(columns.c7_DIST_VIEW_ID) = Me.C7_DIST_VIEW_ID
                OUT_TERM(columns.c8_DIST_VIEW_NAME) = Me.C8_DIST_VIEW_NAME
                OUT_TERM(columns.c9_DIST_COL_ID) = ""
                OUT_TERM(columns.c10_DIST_COL_LEN) = ""
                OUT_TERM(columns.c11_DIST_COL_NAME) = ""
                OUT_TERM(columns.c12_ORG_VIEW_ID) = Me.C12_ORG_VIEW_ID
                OUT_TERM(columns.c13_ORG_VIEW_NAME) = Me.C13_ORG_VIEW_NAME
                OUT_TERM(columns.c14_ORG_COL_ID) = ""
                OUT_TERM(columns.c15_ORG_COL_LEN) = ""
                OUT_TERM(columns.c16_ORG_COL_NAME) = ""
                OUT_TERM(columns.c17_TRANCE_TYPE) = ""
                OUT_TERM(columns.c18_TRANCE_VALUE) = ""
                OUT_TERM(columns.c19_FUNCTION_TYPE) = ""
                OUT_TERM(columns.c20_DIRECT_FLG) = ""
                OUT_TERM(columns.c21_SELECT_CONDITION) = ""
                OUT_TERM(columns.c22_SELECT_OPERATER) = ""
                OUT_TERM(columns.c23_SELECT_TERM) = ""
                OUT_TERM(columns.c24_WORK_DIR) = ""
                OUT_TERM(columns.c25_JOB) = ""
                OUT_TERM(columns.c26_COMMAND) = ""
                '出力行・整形
                strOutLine = getFormatLine(OUT_TERM)
                Return strOutLine

            Case Tag.iLookupViewInfo
                '[LookupViewInfo]ブロック  ビューフィルタ定義
                OUT_TERM(columns.c0_SEQ_NUM) = Me.C0_SEQ_NUM
                OUT_TERM(columns.c1_MODIFY_TIME) = Me.C1_MODIFY_TIME
                OUT_TERM(columns.c2_DIR) = Me.C2_DIR
                OUT_TERM(columns.c3_WTJ_FILE_NAME) = Me.C3_WTJ_FILE_NAME
                'ビューフィルタ区分
                OUT_TERM(columns.c4_VIEW_FILTER_KBN) = Me.C4_VIEW_FILTER_KBN
                'ビューフィルタ区分名称
                OUT_TERM(columns.c5_VIEW_FILTER_KBN_NAME) = Me.C5_VIEW_FILTER_KBN_NAME & "LookupViewInfo"
                OUT_TERM(columns.c6_VIEW_FILTER_NAME) = Me.C6_VIEW_FILTER_NAME
                OUT_TERM(columns.c7_DIST_VIEW_ID) = "-"
                OUT_TERM(columns.c8_DIST_VIEW_NAME) = "参照"
                OUT_TERM(columns.c9_DIST_COL_ID) = ""
                OUT_TERM(columns.c10_DIST_COL_LEN) = ""
                OUT_TERM(columns.c11_DIST_COL_NAME) = ""
                OUT_TERM(columns.c12_ORG_VIEW_ID) = Me.C12_ORG_VIEW_ID
                OUT_TERM(columns.c13_ORG_VIEW_NAME) = Me.C13_ORG_VIEW_NAME
                OUT_TERM(columns.c14_ORG_COL_ID) = ""
                OUT_TERM(columns.c15_ORG_COL_LEN) = ""
                OUT_TERM(columns.c16_ORG_COL_NAME) = ""
                OUT_TERM(columns.c17_TRANCE_TYPE) = ""
                OUT_TERM(columns.c18_TRANCE_VALUE) = ""
                OUT_TERM(columns.c19_FUNCTION_TYPE) = ""
                OUT_TERM(columns.c20_DIRECT_FLG) = ""
                OUT_TERM(columns.c21_SELECT_CONDITION) = ""
                OUT_TERM(columns.c22_SELECT_OPERATER) = ""
                OUT_TERM(columns.c23_SELECT_TERM) = ""
                OUT_TERM(columns.c24_WORK_DIR) = ""
                OUT_TERM(columns.c25_JOB) = ""
                OUT_TERM(columns.c26_COMMAND) = ""

                '出力行・整形
                strOutLine = getFormatLine(OUT_TERM)
                Return strOutLine

            Case Tag.iSelectCondition
                '[SelectCondition]ブロック  ※抽出条件
                OUT_TERM(columns.c0_SEQ_NUM) = Me.C0_SEQ_NUM
                OUT_TERM(columns.c1_MODIFY_TIME) = Me.C1_MODIFY_TIME
                OUT_TERM(columns.c1_MODIFY_TIME) = Me.C1_MODIFY_TIME
                OUT_TERM(columns.c2_DIR) = Me.C2_DIR
                OUT_TERM(columns.c3_WTJ_FILE_NAME) = Me.C3_WTJ_FILE_NAME
                'ビューフィルタ区分
                OUT_TERM(columns.c4_VIEW_FILTER_KBN) = ""
                'ビューフィルタ区分名称
                OUT_TERM(columns.c5_VIEW_FILTER_KBN_NAME) = "抽出条件"
                OUT_TERM(columns.c6_VIEW_FILTER_NAME) = Me.C6_VIEW_FILTER_NAME
                OUT_TERM(columns.c7_DIST_VIEW_ID) = ""
                OUT_TERM(columns.c8_DIST_VIEW_NAME) = ""
                OUT_TERM(columns.c9_DIST_COL_ID) = ""
                OUT_TERM(columns.c10_DIST_COL_LEN) = ""
                OUT_TERM(columns.c11_DIST_COL_NAME) = ""
                OUT_TERM(columns.c12_ORG_VIEW_ID) = Me.C12_ORG_VIEW_ID
                OUT_TERM(columns.c13_ORG_VIEW_NAME) = Me.C13_ORG_VIEW_NAME
                OUT_TERM(columns.c14_ORG_COL_ID) = ""
                OUT_TERM(columns.c15_ORG_COL_LEN) = ""
                OUT_TERM(columns.c16_ORG_COL_NAME) = Me.C16_ORG_COL_NAME
                OUT_TERM(columns.c17_TRANCE_TYPE) = ""
                OUT_TERM(columns.c18_TRANCE_VALUE) = ""
                OUT_TERM(columns.c19_FUNCTION_TYPE) = ""
                OUT_TERM(columns.c20_DIRECT_FLG) = ""
                OUT_TERM(columns.c21_SELECT_CONDITION) = Me.C21_SELECT_CONDITION
                OUT_TERM(columns.c22_SELECT_OPERATER) = Me.C22_SELECT_OPERATER
                OUT_TERM(columns.c23_SELECT_TERM) = Me.C23_SELECT_TERM
                OUT_TERM(columns.c24_WORK_DIR) = ""
                OUT_TERM(columns.c25_JOB) = ""
                OUT_TERM(columns.c26_COMMAND) = ""

                '出力行・整形
                strOutLine = getFormatLine(OUT_TERM)
                Return strOutLine

            Case Tag.iCFValue
                '[CFValue]ブロック  ※アトリビュート（移送元、移送先）
                OUT_TERM(columns.c0_SEQ_NUM) = Me.C0_SEQ_NUM
                OUT_TERM(columns.c1_MODIFY_TIME) = Me.C1_MODIFY_TIME
                OUT_TERM(columns.c2_DIR) = Me.C2_DIR
                OUT_TERM(columns.c3_WTJ_FILE_NAME) = Me.C3_WTJ_FILE_NAME
                'ビューフィルタ区分
                OUT_TERM(columns.c4_VIEW_FILTER_KBN) = Me.C4_VIEW_FILTER_KBN
                'ビューフィルタ区分名称
                OUT_TERM(columns.c5_VIEW_FILTER_KBN_NAME) = Me.C5_VIEW_FILTER_KBN_NAME
                OUT_TERM(columns.c6_VIEW_FILTER_NAME) = Me.C6_VIEW_FILTER_NAME
                OUT_TERM(columns.c7_DIST_VIEW_ID) = Me.C7_DIST_VIEW_ID
                OUT_TERM(columns.c8_DIST_VIEW_NAME) = Me.C8_DIST_VIEW_NAME
                OUT_TERM(columns.c9_DIST_COL_ID) = Me.C9_DIST_COL_ID
                OUT_TERM(columns.c10_DIST_COL_LEN) = Me.C10_DIST_COL_LEN
                OUT_TERM(columns.c11_DIST_COL_NAME) = Me.C11_DIST_COL_NAME
                OUT_TERM(columns.c12_ORG_VIEW_ID) = Me.C12_ORG_VIEW_ID
                OUT_TERM(columns.c13_ORG_VIEW_NAME) = Me.C13_ORG_VIEW_NAME
                OUT_TERM(columns.c14_ORG_COL_ID) = Me.C14_ORG_COL_ID
                OUT_TERM(columns.c15_ORG_COL_LEN) = Me.C15_ORG_COL_LEN
                OUT_TERM(columns.c16_ORG_COL_NAME) = Me.C16_ORG_COL_NAME
                OUT_TERM(columns.c17_TRANCE_TYPE) = Me.C17_TRANCE_TYPE
                OUT_TERM(columns.c18_TRANCE_VALUE) = Me.C18_TRANCE_VALUE
                OUT_TERM(columns.c19_FUNCTION_TYPE) = Me.C19_FUNCTION_TYPE
                OUT_TERM(columns.c20_DIRECT_FLG) = Me.C20_DIRECT_FLG
                OUT_TERM(columns.c21_SELECT_CONDITION) = ""
                OUT_TERM(columns.c22_SELECT_OPERATER) = ""
                OUT_TERM(columns.c23_SELECT_TERM) = ""
                OUT_TERM(columns.c24_WORK_DIR) = ""
                OUT_TERM(columns.c25_JOB) = ""
                OUT_TERM(columns.c26_COMMAND) = ""

                '出力行・整形
                strOutLine = getFormatLine(OUT_TERM)
                Return strOutLine

            Case Tag.iLookupViewInfo
                OUT_TERM(columns.c0_SEQ_NUM) = Me.C0_SEQ_NUM
                OUT_TERM(columns.c1_MODIFY_TIME) = Me.C1_MODIFY_TIME
                OUT_TERM(columns.c2_DIR) = Me.C2_DIR
                OUT_TERM(columns.c3_WTJ_FILE_NAME) = Me.C3_WTJ_FILE_NAME
                'ビューフィルタ区分
                OUT_TERM(columns.c4_VIEW_FILTER_KBN) = Me.C4_VIEW_FILTER_KBN
                'ビューフィルタ区分名称
                OUT_TERM(columns.c5_VIEW_FILTER_KBN_NAME) = Me.C5_VIEW_FILTER_KBN_NAME
                OUT_TERM(columns.c6_VIEW_FILTER_NAME) = Me.C6_VIEW_FILTER_NAME
                OUT_TERM(columns.c7_DIST_VIEW_ID) = Me.C7_DIST_VIEW_ID
                OUT_TERM(columns.c8_DIST_VIEW_NAME) = Me.C8_DIST_VIEW_NAME
                OUT_TERM(columns.c9_DIST_COL_ID) = ""
                OUT_TERM(columns.c10_DIST_COL_LEN) = ""
                OUT_TERM(columns.c11_DIST_COL_NAME) = ""
                OUT_TERM(columns.c12_ORG_VIEW_ID) = Me.C12_ORG_VIEW_ID
                OUT_TERM(columns.c13_ORG_VIEW_NAME) = Me.C13_ORG_VIEW_NAME
                OUT_TERM(columns.c14_ORG_COL_ID) = ""
                OUT_TERM(columns.c15_ORG_COL_LEN) = ""
                OUT_TERM(columns.c16_ORG_COL_NAME) = ""
                OUT_TERM(columns.c17_TRANCE_TYPE) = ""
                OUT_TERM(columns.c18_TRANCE_VALUE) = ""
                OUT_TERM(columns.c19_FUNCTION_TYPE) = ""
                OUT_TERM(columns.c20_DIRECT_FLG) = ""
                OUT_TERM(columns.c21_SELECT_CONDITION) = ""
                OUT_TERM(columns.c22_SELECT_OPERATER) = ""
                OUT_TERM(columns.c23_SELECT_TERM) = ""
                OUT_TERM(columns.c24_WORK_DIR) = ""
                OUT_TERM(columns.c25_JOB) = ""
                OUT_TERM(columns.c26_COMMAND) = ""

                '出力行・整形
                strOutLine = getFormatLine(OUT_TERM)
                Return strOutLine

            Case Tag.iProcess
                OUT_TERM(columns.c0_SEQ_NUM) = Me.C0_SEQ_NUM
                OUT_TERM(columns.c1_MODIFY_TIME) = Me.C1_MODIFY_TIME
                OUT_TERM(columns.c2_DIR) = Me.C2_DIR
                OUT_TERM(columns.c3_WTJ_FILE_NAME) = Me.C3_WTJ_FILE_NAME
                'ビューフィルタ区分
                OUT_TERM(columns.c4_VIEW_FILTER_KBN) = Me.C4_VIEW_FILTER_KBN
                'ビューフィルタ区分名称
                OUT_TERM(columns.c5_VIEW_FILTER_KBN_NAME) = Me.C5_VIEW_FILTER_KBN_NAME
                OUT_TERM(columns.c6_VIEW_FILTER_NAME) = Me.C6_VIEW_FILTER_NAME
                OUT_TERM(columns.c7_DIST_VIEW_ID) = Me.C7_DIST_VIEW_ID
                OUT_TERM(columns.c8_DIST_VIEW_NAME) = Me.C8_DIST_VIEW_NAME
                OUT_TERM(columns.c9_DIST_COL_ID) = ""
                OUT_TERM(columns.c10_DIST_COL_LEN) = ""
                OUT_TERM(columns.c11_DIST_COL_NAME) = ""
                OUT_TERM(columns.c12_ORG_VIEW_ID) = ""
                ' Me.C12_ORG_VIEW_ID
                OUT_TERM(columns.c13_ORG_VIEW_NAME) = ""
                'Me.C13_ORG_VIEW_NAME
                OUT_TERM(columns.c14_ORG_COL_ID) = ""
                OUT_TERM(columns.c15_ORG_COL_LEN) = ""
                OUT_TERM(columns.c16_ORG_COL_NAME) = ""
                OUT_TERM(columns.c17_TRANCE_TYPE) = ""
                OUT_TERM(columns.c18_TRANCE_VALUE) = ""
                OUT_TERM(columns.c19_FUNCTION_TYPE) = ""
                OUT_TERM(columns.c20_DIRECT_FLG) = ""
                OUT_TERM(columns.c21_SELECT_CONDITION) = ""
                OUT_TERM(columns.c22_SELECT_OPERATER) = ""
                OUT_TERM(columns.c23_SELECT_TERM) = ""
                OUT_TERM(columns.c24_WORK_DIR) = Me.C24_WORK_DIR
                OUT_TERM(columns.c25_JOB) = Me.C25_JOB
                OUT_TERM(columns.c26_COMMAND) = Me.C26_COMMAND

                '出力行・整形
                strOutLine = getFormatLine(OUT_TERM)
                Return strOutLine

            Case Tag.iNone
                Return Nothing
        End Select

        Return Nothing

    End Function


    Public Sub checkPrint(iStatus As UInteger)
        Dim line As String = Me.getLine(iStatus)
    End Sub


    Private Function getFormatLine(columnArray() As String)
        Dim csvForm As String = ""
        For Each col As String In columnArray
            csvForm = csvForm & col & ","
        Next

        Return csvForm

    End Function

End Class
