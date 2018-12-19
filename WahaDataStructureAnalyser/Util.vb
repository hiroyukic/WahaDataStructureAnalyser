Public Class Util


#Region "　Left メソッド　"

    ''' -----------------------------------------------------------------------------------
    ''' <summary>
    '''     文字列の左端から指定された文字数分の文字列を返します。</summary>
    ''' <param name="stTarget">
    '''     取り出す元になる文字列。</param>
    ''' <param name="iLength">
    '''     取り出す文字数。</param>
    ''' <returns>
    '''     左端から指定された文字数分の文字列。
    '''     文字数を超えた場合は、文字列全体が返されます。</returns>
    ''' -----------------------------------------------------------------------------------
    Public Shared Function Left(ByVal stTarget As String, ByVal iLength As Integer) As String
        If iLength <= stTarget.Length Then
            Return stTarget.Substring(0, iLength)
        End If

        Return stTarget
    End Function

#End Region

#Region "　Mid メソッド (+1)　"

    ''' -----------------------------------------------------------------------------------
    ''' <summary>
    '''     文字列の指定された位置以降のすべての文字列を返します。</summary>
    ''' <param name="stTarget">
    '''     取り出す元になる文字列。</param>
    ''' <param name="iStart">
    '''     取り出しを開始する位置。</param>
    ''' <returns>
    '''     指定された位置以降のすべての文字列。</returns>
    ''' -----------------------------------------------------------------------------------
    Public Overloads Shared Function Mid(ByVal stTarget As String, ByVal iStart As Integer) As String
        If iStart <= stTarget.Length Then
            Return stTarget.Substring(iStart - 1)
        End If

        Return String.Empty
    End Function

    ''' -----------------------------------------------------------------------------------
    ''' <summary>
    '''     文字列の指定された位置から、指定された文字数分の文字列を返します。</summary>
    ''' <param name="stTarget">
    '''     取り出す元になる文字列。</param>
    ''' <param name="iStart">
    '''     取り出しを開始する位置。</param>
    ''' <param name="iLength">
    '''     取り出す文字数。</param>
    ''' <returns>
    '''     指定された位置から指定された文字数分の文字列。
    '''     文字数を超えた場合は、指定された位置からすべての文字列が返されます。</returns>
    ''' -----------------------------------------------------------------------------------
    Public Overloads Shared Function Mid(ByVal stTarget As String, ByVal iStart As Integer, ByVal iLength As Integer) As String
        If iStart <= stTarget.Length Then
            If iStart + iLength - 1 <= stTarget.Length Then
                Return stTarget.Substring(iStart - 1, iLength)
            End If

            Return stTarget.Substring(iStart - 1)
        End If

        Return String.Empty
    End Function

#End Region

#Region "　Right メソッド　"

    ''' -----------------------------------------------------------------------------------
    ''' <summary>
    '''     文字列の右端から指定された文字数分の文字列を返します。</summary>
    ''' <param name="stTarget">
    '''     取り出す元になる文字列。</param>
    ''' <param name="iLength">
    '''     取り出す文字数。</param>
    ''' <returns>
    '''     右端から指定された文字数分の文字列。
    '''     文字数を超えた場合は、文字列全体が返されます。</returns>
    ''' -----------------------------------------------------------------------------------
    Public Shared Function Right(ByVal stTarget As String, ByVal iLength As Integer) As String
        If iLength <= stTarget.Length Then
            Return stTarget.Substring(stTarget.Length - iLength)
        End If

        Return stTarget
    End Function

#End Region













End Class
