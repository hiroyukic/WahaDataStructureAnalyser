Public Class Form1

    'カレントディレクトリ
    Private strCurrentDir As String = System.IO.Directory.GetCurrentDirectory()
    'For TEST
    'Private strCurrentDir As String = "D:\Work0\Waha!WTJ_20160405"



    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        ' カレントディレクトリを取得する
        'Dim stCurrentDir As String = System.IO.Directory.GetCurrentDirectory()
        TextBox1.Text = strCurrentDir

        'ListBox1.ItemHeight = 25
        'ListBox1.AutoSize = True

    End Sub




    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click


        'カーソル（Wait)
        Me.Cursor = Cursors.WaitCursor

        'カレントディレクトリ以下のファイルをすべて取得する
        'ワイルドカード"*"は、すべてのファイルを意味する
        Dim files As String() = System.IO.Directory.GetFiles(strCurrentDir, "*.wtj", System.IO.SearchOption.AllDirectories)

        'ListBox1に結果を表示する
        'ListBox1.Items.AddRange(files)
        Console.Write(files.Length)

        '出力ファイル名称生成（タイムスタンプ付加）
        Dim timeStampNow As DateTime = System.DateTimeOffset.Now.ToString
        'Dim t = String.Format("{0:yyyyMMdd_HHmmss}", timeStampNow)
        Dim strTimeStampNow = String.Format("{0:yyyyMMdd_HHmm}", timeStampNow)
        Dim OUTPUT_FILENAME = Me.strCurrentDir & "\wtjStructData_" & strTimeStampNow & ".csv"

        'カレントディレクトリパスの表示

        'Me.TextBox1.Items.Add("[CurrentDir] " & strCurrentDir)
        'TextBox1.Items.Add("")
        'カレントディレクトリ文字列の長さ
        Dim iLength As Integer = strCurrentDir.Length()

        'ファイル名称リスト 表示
        Dim analyser = New SyntacticAnalysis()
        Dim strFileName = ""
        For i = 0 To files.Length - 1
            'wtjファイル名取得
            strFileName = Util.Mid(files(i), iLength + 1)
            '1行（wtjファイル名）追加
            'ListBox1.Items.Add(i & strFileName)

            Debug.WriteLine(files(i))

            'WTJデータ構造解析
            '■■■■■■■■■■■■■■■■■■■■
            'analyser.Conversion(files(i))
            analyser.Conversion(files(i), OUTPUT_FILENAME)
            '■■■■■■■■■■■■■■■■■■■■
        Next


        'Me.TextBox1.Text("ddd")

        'ListBox1.Items.Add("終了しました。以下のファイルに出力しました。")
        'ListBox1.Items.Add("[解析結果出力先] " & OUTPUT_FILENAME)

        'カーソル（Default)
        Me.Cursor = Cursors.Default

    End Sub

    Private Sub ListBox1_SelectedIndexChanged(sender As Object, e As EventArgs)

    End Sub
End Class

