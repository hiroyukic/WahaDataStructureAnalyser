package wtj;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.LineNumberReader;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Hashtable;
import java.util.regex.Pattern;

/**
 * 「WTJプログラム・解析ツール」
 *
 *  ※設定値中の",(comma)"、ならびに、""(double quatation)"は、出力ファイルに
 *  　おいてそれぞれ、"、"、"”"に変換する。
 *
 * 《出力フォーマット (20130604)》
 *  1 シーケンス番号	　数値文字列	15		許可	※出力シーケンス(20130604 新規追加)
 *  2 タイムスタンプ	　数値文字列	15		許可	※20130418 新規追加
 *  3 WTJファイル名	　　　　　　文字	128		許可	WTJファイル名称
 *  4 ビューフィルタ種別	　　数値	2	0	許可	0：標準, 1：結合, 2：統合, 4：, 5：参照
 *  5 ビューフィルタ種別名称	文字	8		許可
 *  6 ビューフィルタ名称	　　文字	99		許可	ViewFilter名称
 *  7 移送先ビューID	　数値文字列	3	0	許可	移送先・ビューID
 *  8 移送先ビューテーブル名称	文字	99		許可
 *  9 移送先カラムID	　数値文字列	4	0	許可	移送先・カラムID
 * 10 移送先カラム長	　数値文字列	4	0	許可
 * 11 移送先カラム名称	　　　　文字	99		許可	移送先・カラム名称
 * 12 移送元ビューID	　数値文字列	3	0	許可	移送元・ビューID
 * 13 移送元ビューテーブル名称	文字	99		許可
 * 14 移送元カラムID	　数値文字列	5	0	許可	移送元・カラムID
 * 15 移送元カラム長	　数値文字列	4	0	許可
 * 16 移送元カラム名称	　　　　文字	256		許可	移送元・カラム名称
 * 17 移送値型	　　　　　数値文字列	3	0	許可	Valタイプフラグ
 * 18 移送値	　　　　　　　　文字	256		許可	実値(Value)
 * 19 関数タイプ	　　　数値文字列	4	0	許可	関数ID
 * 20 直接移送フラグ	　数値文字列	1	0	許可	１：直接移送
 * 21 抽出条件（値）	　　　　文字	32		許可
 * 22 抽出条件（演算子）	　　文字	8		許可
 * 23 抽出条件（項目）	　　　　文字	64		許可
 *
 *
 * @author hiroyuki-c
 * @date 2011/04/01
 * Update 2013/04/19 出力項目追加（格納フォルダ（業務名））
 * Update 2015/04/06 wtjのバージョンチェックロジック追加（Repository）
 * Update 2016/09/23 wtjのバージョンチェックロジック追加（Process)
 * Update 2016/10/04 wtjのバージョンチェックロジック追加（ViewInfo(DivideInfo):「分割」「統合」「集計」)
 * Update 2016/10/04 wtjのバージョンチェックロジック追加（LookupViewInfo:「参照」)
 * Update 2016/10/07 出力ファイル名変更（"yyyyMMddHHmmss"→"yyyyMMdd"）cf.getDateStamp()
 *
 */
public class WTJAnalyze {
	// 出力ファイル
	private static String WRITE_PATH = "";
	private static PrintWriter PW = null;

	// ログファイル
	private static String LOG_PATH = "";
	private static PrintWriter PW_LOG = null;

	//Message
	private static String msg = null;

	// 全レコードカウンター（シーケンス番号）
	private static long seqNo = 1;

	// タイトル行
	private static String TITLE = "SeqNo,"
			+ "タイムスタンプ,"
			+ "格納フォルダ（業務名）,"
			+ "WTJファイル名,"
			+ "ビューフィルタ種別,"
			+ "ビューフィルタ種別名称,"
			+ "ビューフィルタ名称,"
			+ "移送先ビューID,"
			+ "移送先ビューテーブル名称,"
			+ "移送先カラムID,"
			+ "移送先カラム長,"
			+ "移送先カラム名称,"
			+ "移送元ビューID,"
			+ "移送元ビューテーブル名称,"
			+ "移送元カラムID,"
			+ "移送元カラム長,"
			+ "移送元カラム名称,"
			+ "移送値型,"
			+ "移送値,"
			+ "関数タイプ,"
			+ "直送フラグ,"
			+ "抽出条件（条件値）,"
			+ "抽出条件（比較演算子）,"
			+ "抽出条件（項目）,"
			+ "WorkDir,"
			+ "処理（0:前／1:後),"
			+ "CommandLine";

	// ハッシュ：<Waha関数ID>,<関数名称>
	private static Hashtable<Integer, String> functionNameHash = new Hashtable<Integer, String>();
	// ハッシュ：<ビューフィルタID>,<ビューフィルタ名称>
	private static Hashtable<Integer, String> viewFilterNameHash = new Hashtable<Integer, String>();
	// ハッシュ：<二項比較子ID>,<記号名称>
	private static Hashtable<Integer, String> compOpeNameHash = new Hashtable<Integer, String>();

	//20120529 「View」：テーブル定義名称を格納する。※IDをインデックスとしたストリング名称を格納する。
	//private static String viewTable[] ;
	//private static String[] viewTable ;
	// ハッシュ：<二項比較子ID>,<記号名称>
	private static Hashtable<String, String> ViewTableNameHash = new Hashtable<String, String>();

	public WTJAnalyze(String path) {
		try {
			//Waha関数名称取得用ハッシュの初期化
			initFunctionList();

			seqNo = 1;	//20161006

			// 出力ファイル生成
			WRITE_PATH = ".\\WTJStructureData_" + getDateStamp() + ".csv";
			PW = new PrintWriter(new BufferedWriter(new FileWriter(WRITE_PATH)));
			PW.println(TITLE);

			// ログファイル生成
			LOG_PATH = ".\\WTJStructureData_" + getDateStamp() + ".log";
			PW_LOG = new PrintWriter(new BufferedWriter(new FileWriter(LOG_PATH)));

			/*
			 *  WTJデータ解析再帰処理
			 */
			File dir = new File(path);

			if(dir.isDirectory()) {
				//再帰処理
				ListPath(dir);
			} else {
				msg = "[Error]:指定されたディレクトリパスは存在しません。";
				System.out.println(msg);
				PW_LOG.println(msg);
			}

		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			System.out.println(">END.");
			try {
				PW.close();
				PW_LOG.close();
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
	}

	/**
	 * main
	 *
	 * @param args
	 */
	public static void main(String[] args) {
		// -----------------------------------------------
		// 解析開始フォルダ（デフォルト：jar起動フォルダ）
		// -----------------------------------------------
		String path = ".\\";

		//解析開始
		WTJAnalyze runner = new WTJAnalyze(path);
	}

	/**
	 * フォルダパスを受け、指定フォルダ内を再帰検索する。
	 *
	 * @param dir
	 */
	private static void ListPath(File dir) {
		File[] fs = dir.listFiles();

		for (int i = 0; i < fs.length; i++) {
			File f = fs[i];
			if (f.isDirectory()) {
				ListPath(f);
			} else if (f.isFile()) {
				try {
					if (f.isFile())
						// WTJデータ構造分析開始
						Analyze(f.getCanonicalPath());
				} catch (IOException ex) {
					// do nothing
				}
			}
		}
	}

	/**
	 * 指定のファイルを読込み、WTJデータのパース処理を行う。
	 *
	 * @param target_file_path Analyze
	 */
	private static void Analyze(String target_file_path) {
		String READ_PATH = target_file_path;
		String modifyTime = ""; //タイムスタンプ
		String parentDir = ""; //格納フォルダ名称（業務名）

		// 拡張子(wtj)以外は処理しない。
		if (getSuffix(READ_PATH).matches("wtj")) {
			// continue
		} else {
			// 処理対象外ファイル
			return;
		}
		String fileNameBody = getFileNameBody(READ_PATH);

		// ログファイルに出力
		PW_LOG.println("■READ_PATH=" + READ_PATH);

		// ファイルREADER初期化
		FileReader fr = null;
		LineNumberReader lnr = null;

		// String regex = "\\b([A-Za-z_]\\w*)\\.(java|class)\\b";
		String reg_Column = "^\\[Column\\]";
		Pattern pat_Column = Pattern.compile(reg_Column);
		Pattern pat_View = Pattern.compile("^\\[View\\]");
		Pattern pat_VFilter = Pattern.compile("^\\[VFilter\\]");
		Pattern pat_CFilter = Pattern.compile("^\\[CFilter\\]");
		Pattern pat_CFValue = Pattern.compile("^\\[CFValue\\]");
		Pattern pat_Otherx = Pattern.compile("^\\[");
		//20120622
		Pattern pat_SelectCondition = Pattern.compile("^\\[SelectCondition\\]");
		//20130415
		Pattern pat_DViewSelectCondition = Pattern.compile("^\\[DViewSelectCondition\\]");
		Pattern pat_DeleteSelectCondition = Pattern.compile("^\\[DeleteSelectCondition\\]");

		//20150406
		Pattern pat_Repository = Pattern.compile("^\\[Repository\\]");
		//20160923
		Pattern pat_Process = Pattern.compile("^\\[Process\\]");
		//20161004
		Pattern pat_ViewInfo = Pattern.compile("^\\[ViewInfo\\]");
		//20161004 参照
		Pattern pat_LookupViewInfo = Pattern.compile("^\\[LookupViewInfo\\]");

		try {
			// ファイルのタイムスタンプ
			modifyTime = getFileLastModifyTime(READ_PATH);

			// 格納フォルダ名称（業務名）add.20130419
			parentDir = getParentDir(READ_PATH);

			fr = new FileReader(READ_PATH);
			lnr = new LineNumberReader(fr);

			/*
			 * Hashtable numbers = new Hashtable(); numbers.put("one", new
			 * Integer(1)); numbers.put("two", new Integer(2));
			 * numbers.put("three", new Integer(3));
			 *
			 * Integer n = (Integer)numbers.get("two"); if (n != null) {
			 * System.out.println("two = " + n); }
			 */

			// 属性情報保存用ハッシュテーブル
			Hashtable<String, String> columnHash = new Hashtable<String, String>();
			Hashtable<String, String> columnHashLength = new Hashtable<String, String>();

			// [View]
			String viewID = null;

			// [Column]
			String columnName = null;
			String columnID = null;
			String columnLength = null;

			// [VFilter]
			String[] vFilterAttrList = null;
			String vFilterKind = null;
			String vFilterKindName = null;
			String vFilterID = null;
			String vFilterName = null;

			// [CFilter]
			String cFilterOutViewID = null;
			String cFilterOutColID = null;

			// [CFValue]
			String cFValueViewID = null;
			String cFValueValue = null;
			String cFValueSFType = null;
			String cFValueRealValueType = null;
			String cFValuelikColID = null;
			String cFValueID = null;
			String cFValueName = null;
			String cFValueLength = null;

			String cFValueIsRoot = null;
			String cFValueChildIDs = null;

			String cFValueValueType = null;
			String cFValueSetValueType = null;
			String cFValueObjType = null;

			// [SelectCondition]
			String cSelectConditionViewID = null;
			String cSelectConditionValue = null;
			String cSelectConditionOperator = null;
			String cSelectConditionColumnID = null;

			// [Prosess] コマンドライン
			String cDicision = null;
			String cWorkDir = null;
			String cCommandLine = null;

			// [ViewInfo] コマンドライン
			String cViewKind = null;
			//String cCommandLine.
			String cViewID= null;
			String inputViewID = null;
			String inputView = null;

			// 構文ステータス
			int status = 0;
			// ブロック内・行カウンター
			int blockLineCount = 0;
			// 読込み行
			String line = null;
			while ((line = lnr.readLine()) != null) {
				//seqNo++; //シーケンス番号
				String x = line.substring(0, 1);
				if (pat_View.matcher(line).matches()) {
					// [View]
					status = 1;
					blockLineCount = 0;
				} else if (pat_Column.matcher(line).matches()) {
					// [Column]
					status = 2;
					blockLineCount = 0;
				} else if (pat_VFilter.matcher(line).matches()) {
					// [VFilter]
					status = 3;
					blockLineCount = 0;
				} else if (pat_CFilter.matcher(line).matches()) {
					// [CFilter]
					status = 4;
					blockLineCount = 0;
				} else if (pat_CFValue.matcher(line).matches()) {
					// [CFValue]
					status = 5;
					blockLineCount = 0;
				} else if (pat_SelectCondition.matcher(line).matches()) {
					// [SelectCondition]
					status = 6;
					blockLineCount = 0;

				} else if (pat_DViewSelectCondition.matcher(line).matches()) {
					// [DViewSelectCondition]
					status = 7;
					blockLineCount = 0;
				} else if (pat_DeleteSelectCondition.matcher(line).matches()) {
					// [DeleteSelectCondition]
					status = 8;
					blockLineCount = 0;

				//20141222 add Waha バージョン
				} else if (pat_Repository.matcher(line).matches()) {
					// [Repository]
					status = 9;
					blockLineCount = 0;

				//20160923 add Waha バージョン
				} else if (pat_Process.matcher(line).matches()) {
					// [Repository]
					status = 10;
					blockLineCount = 0;

				//20161004 「分割」「標準」「集計」[ViewInfo]
				} else if (pat_ViewInfo.matcher(line).matches()) {
					// [ViewInfo]
					status = 11;
					blockLineCount = 0;

				//20161004 「参照」[LookupViewInfo]
				} else if (pat_LookupViewInfo.matcher(line).matches()) {
					// [LookupViewInfo]
					status = 12;
					blockLineCount = 0;  //TODO ?
					//System.out.println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>12");

				} else if (pat_Otherx.matcher(x).matches()) {
					//処理対象外
					status = 999;
					blockLineCount = 0;
				}

				// ファイル出力文字列初期化
				String outLineStr = null;

				switch (status) {
				case 1:
					// [View] ブロック処理
					blockLineCount += 1;
					// データレコード時（3行目）の処理
					if (blockLineCount == 3) {
						String[] columnsList = line.split("\t");
						viewID = columnsList[7].trim();

						//20120529 テーブル名称（FileName)をハッシュ格納
						//int vid = (int)Integer.valueOf(viewID);
						//String vn = new String(columnsList[5].trim());
						//viewTable[vid] = vn;
						//ViewTableNameHash.put(viewID , columnsList[5].trim());
						ViewTableNameHash.put(viewID , columnsList[8].trim());
					}
					break;

				case 2:
					// [Column]ブロック処理
					blockLineCount += 1;
					// データレコード時（3行目以上）の処理
					if (blockLineCount >= 3) {
						String[] columnsList = line.split("\t");
						// 項目名称
						columnName = columnsList[5].trim();
						// カラムID
						columnID = columnsList[10].trim();
						// Length
						columnLength = columnsList[6].trim();
						// ViewIDとColumnIDをキーとし項目名称をハッシュに入れる
						columnHash.put(viewID + "_" + columnID, columnName);
						columnHashLength.put(viewID + "_" + columnID, columnLength);
					}
					break;

				case 3:
					// [VFilter] ブロック処理
					blockLineCount += 1;
					// データレコード時（2行目）の処理
					if (blockLineCount == 2) {
						// タイトルレコードから属性名の配列("Kind","ID","ID")を作成。取り置きする。
						vFilterAttrList = line.split("\t");
					} else if (blockLineCount == 3) {
						// データレコード時（3行目）の処理
						String[] columnsList = line.split("\t");
						vFilterKind = columnsList[getKeysindex(vFilterAttrList,
								"Kind")];
						//ビューフィルタ名称をハッシュより取得
						vFilterKindName =getViewFilterName(string2Number(vFilterKind));
						vFilterID = columnsList[getKeysindex(vFilterAttrList, "ID")];
						vFilterName = columnsList[getKeysindex(vFilterAttrList, "Name")];
					}
					break;

				case 4:
					// [CFilter] ブロック処理
					blockLineCount += 1;
					// データレコード時（3行目）の処理
					if (blockLineCount == 3) {
						String[] columnsList = line.split("\t");
						// 移送先・属性IDの取得
						cFilterOutViewID = columnsList[0].trim();
						cFilterOutColID = columnsList[1].trim();
						// cFilterFileName = columnsList[8].trim();
						// cFilterName = columnsList[5].trim();
					}
					break;

				case 5:
					// [CFValue] ブロック処理
					blockLineCount += 1;
					// データレコード時（3行目以降）の処理
					if (blockLineCount >= 3) {
						// レコード行の分割（配列）
						String[] columnsList = line.split("\t");
						// 移送元・属性情報の取得
						cFValueViewID = columnsList[0].trim(); 			// 移送元・ビューID
						cFValueValue = columnsList[2].trim(); 			// 移送元・移送値
						cFValueSFType = columnsList[4].trim(); 			// 関数タイプ
						cFValueRealValueType = columnsList[5].trim(); 	// 実値型
						cFValuelikColID = columnsList[9].trim(); 		// 移送元・カラムID
						cFValueID = columnsList[11].trim();
						cFValueIsRoot = columnsList[10].trim(); 		// ルートフラグ
						cFValueChildIDs = columnsList[13].trim(); 		// パラメータ
						//二項比較子
						cFValueValueType = columnsList[1].trim();    	//ValueType
						cFValueSetValueType = columnsList[3].trim(); 	//SetValueType
						cFValueObjType = columnsList[6].trim();      	//ObjType

						// 転送元・項目名称
						if (cFValueViewID.equals("0")
								&& cFValuelikColID.equals("0")) {
							cFValueLength = "0";
							int functionNumber = string2Number(cFValueSFType);
							if (functionNumber > 0) {
								//Waha関数の設定
								String functionName = getFunctionName(functionNumber);
								if(functionName.length() > 0){
									//関数名が登録している場合
									cFValueName = "■Function[" + functionName + "]";
								} else {
									//関数名が登録されていない場合、関数IDを出力
									cFValueName = "■Function[" + cFValueSFType + "]";
								}
							} else if (string2Number(cFValueRealValueType) > 0) {
								// カンマを置換（CSV出力ファイルに影響するため）
								cFValueValue = cFValueValue .replaceAll(",", "、");
								// ダブルクオーテーションを置換（CSV出力ファイルに影響するため）
								cFValueValue = cFValueValue .replaceAll("\"", "”");

								cFValueName = "□Value[" + cFValueValue + "]";
							} else if (cFValueValueType.matches("6") &&  cFValueSetValueType.matches("101") && cFValueObjType.matches("5") 	) {
								// 1:≠, 2:＜, 3:＞, 4:≦, 5:≧, 6:中間一致, 7:前方一致, 8:後方一致
								String operatorName = getOperatorName(cFValueValue);
								cFValueValue = "";
								cFValueName = "□Ope[" + operatorName + "]";
							} else if (cFValueValueType.matches("1") &&  cFValueSetValueType.matches("8") && cFValueObjType.matches("4") 	) {
								// Null
								cFValueValue = "";
								cFValueName = "□Value[Null]";
							} else {
								// 解析不定分（リセット条件[Numberring]、削除等）
								cFValueValue = "";
								cFValueName = "□未設定";
							}
						} else {
							//転送元情報取得
							cFValueName = columnHash.get(cFValueViewID + "_"
									+ cFValuelikColID);
							cFValueLength = columnHashLength.get(cFValueViewID
									+ "_" + cFValuelikColID);
						}

						// 直接移送の検知 （IsRoot=1 かつ、ChilsIds=0の場合、"直接移送"とみなす）
						String directFlg = "0";
						if (cFValueIsRoot.equals("1")
								&& cFValueChildIDs.equals("0")) {
							directFlg = "1";
						}

						// -----------------------------------------------
						// データ構造出力
						// -----------------------------------------------
						outLineStr = seqNo++
								+ ","
								+ modifyTime         // タイムsタンプ（20130418)
								+ ","
								+ parentDir				// 業務名称（親フォルダ名称）

								+ ","
								+ fileNameBody
								+ ","
								+ vFilterKind 			// ビューフィルタ区分
								+ ","
								+ vFilterKindName 		// ビューフィルタ区分名称：TODO 2011/04/01
								+ ","
								+ vFilterName 			// ビューフィルタ名称
								+ ","
								+ cFilterOutViewID 		// 転送先・ビューID
								+ ","

								//20120529
								+ ViewTableNameHash.get(cFilterOutViewID)  // 転送先・Viewテーブル名称

								+ ","
								+ cFilterOutColID 		// 転送先・カラムID
								+ ","
								+ columnHashLength.get(cFilterOutViewID + "_"
										+ cFilterOutColID) 		// 転送先・項目長
								+ ","
								+ columnHash.get(cFilterOutViewID + "_" + cFilterOutColID) + "," // 転送先・項目名称
								+ cFValueViewID + "," 	// 転送元・ビューID

								//20120529
								+ ViewTableNameHash.get(cFValueViewID) + "," // 転送元・Viewテーブル名称

								+ cFValuelikColID + "," // 転送元・カラムID
								+ cFValueLength + "," 	// 転送元・カラム長
								+ cFValueName + "," 	// 転送元・カラム名称

								+ cFValueRealValueType + "," 	// 実値型
								+ "\"" + cFValueValue + "\"," 	// 設定値
								+ cFValueSFType + "," 	// 関数タイプ
								+ directFlg; 			// 直接移送フラグ（1：直接移送）
						// ファイル出力
						System.out.println(outLineStr);
						PW.println(outLineStr);
					}
					break;

				//----------------------------------------------------------------------------
				// Select
				//----------------------------------------------------------------------------
				case 6:
				case 7:
				case 8:
					// [Select][DViewSelectCondition][DeleteSelectCondition] ブロック処理
					blockLineCount += 1;
					// データレコード時（3行目以降）の処理
					if (blockLineCount >= 3) {
							// レコード行の分割（配列）
							String[] columnsList = line.split("\t");
							// 移送元・属性情報の取得
							cSelectConditionViewID = columnsList[0].trim(); 		// 参照ビューID
							cSelectConditionValue = columnsList[2].trim(); 			// 設定値
							cSelectConditionOperator = columnsList[3].trim(); 		// オペレーター
							cSelectConditionColumnID = columnsList[10].trim(); 		// 参照テーブル・カラムID

							// 1:≠, 2:＜, 3:＞, 4:≦, 5:≧, 6:中間一致, 7:前方一致, 8:後方一致
							String operatorName = getOperatorName(cSelectConditionOperator);
							// -----------------------------------------------
							// データ構造出力 （抽出条件）
							// -----------------------------------------------
							outLineStr = seqNo++
									+ ","
									+ modifyTime

									+ ","
									+ parentDir				// 業務名称（親フォルダ名称）

									+ ","
									+ fileNameBody
									+ ","
									+ ","
									+ "抽出条件,"
									+ vFilterName 			// ビューフィルタ名称
									+ ","
									+ ","
									+ ","
									+ ","
									+ ","
									+ ","
									+ ","
									+ ViewTableNameHash.get(cSelectConditionViewID) + "," // Viewテーブル名称
									+ ","
									+ ","
									+ columnHash.get(cSelectConditionViewID + "_" + cSelectConditionColumnID)  // 属性名称
									+ ","
									+ ","
									+ ","
									+ ","
									+ ","
									+ cSelectConditionValue 		// 抽出条件値、環境変数
									+ ","
									+ operatorName					// オペレーター
									+ ","
									+ columnHash.get(cSelectConditionViewID + "_" + cSelectConditionColumnID)  // 属性名称
									;

							// ファイル出力
							System.out.println(outLineStr);
							PW.println(outLineStr);
					}
					break;

				case 9:
					// [RepositoryVersion] ブロック処理
					blockLineCount += 1;
					// データレコード時（3行目）の処理
					if (blockLineCount == 3) {
						String[] columnsList = line.split("\t");
						// "バージョン" の取得
						String repositoryVersion;
						repositoryVersion = columnsList[1].trim();
						if(repositoryVersion.startsWith("2") == false) {
							//msg = "\n\r[WahaVersionCheckError]: WTJファイルは、Waha [" + repositoryVersion + " ] です。バージョン対象外のため正しい解析ができません。\n\r" + READ_PATH + "\n\r";
							msg = "\r[WahaVersionCheckError]: WTJファイルは、Waha [" + repositoryVersion + " ] です。バージョン対象外のため正しい解析ができません。\r" + READ_PATH + "\r";
							System.out.println(msg);
							PW_LOG.println(msg);
							PW.println(msg);
							msg = "\n[WahaVersionCheckError]: Wahaバージョン対象外の WTJファイル があります。ログを確認して下さい。\n\r";
						}
					}
					break;

				case 10:
					// [Process] ブロック処理 / 2016.09.23
					blockLineCount += 1;
					// データレコード時（3行目以降）の処理
					if (blockLineCount > 2 ) {
							// レコード行の分割（配列）
							String[] columnsList = line.split("\t");
							// 移送元・属性情報の取得
							cWorkDir = columnsList[0].trim(); 		// WorkDir
							cDicision = columnsList[11].trim(); 		// 前処理[0]、後処理[1]
							cCommandLine = columnsList[12].trim(); 		// CommandLine

							// -----------------------------------------------
							// データ構造出力 （コマンドライン文字列）
							// -----------------------------------------------
							outLineStr = seqNo++
									+ ","
									+ modifyTime
									+ ","
									+ parentDir				// 業務名称（親フォルダ名称）
									+ ","
									+ fileNameBody

									+ ","
									+ vFilterKind 			// ビューフィルタ区分
									+ ","
									+ vFilterKindName 		// ビューフィルタ区分名称：TODO 2011/04/01
									+ ","
									+ vFilterName 			// ビューフィルタ名称
									+ ","
									+ cFilterOutViewID 		// 転送先・ビューID
									+ ","
									+ ViewTableNameHash.get(cFilterOutViewID)  // 転送先・Viewテーブル名称
									+ ","
									+ ","
									+ ","
									+ ","
									+ ","
									+ "," // Viewテーブル名称
									+ ","
									+ ","
									+ ","
									+ ","
									+ ","
									+ ","
									+ ","
									+ ","
									+ ","
									+ ","
							+ cWorkDir
							+ ","
							+ cDicision
							+ ","
							+ "CMDLINE→" + cCommandLine
							;

							// ファイル出力
							System.out.println(outLineStr);
							PW.println(outLineStr);
					}
					break;

				case 11:	//「分割」「標準」 20161004
					// 標準：入力ファイル　１、出力ファイル　１
					// 分割：入力ファイル　１、出力ファイル　複数
					// [ViewInfo] ブロック処理
					blockLineCount += 1;
					// データレコード時（3行目以降）の処理
					if (blockLineCount > 2 ) {
							// レコード行の分割（配列）
							String[] columnsList = line.split("\t");
							// 入力ファイル／分割ファイル名称の取得
							cViewKind = columnsList[0].trim(); 		// 0:入力ファイル、1:分割ファイル
							cViewID = columnsList[1].trim(); 		// 前処理[0]、後処理[1]

							String outputViewID = null;
							String outputView = null;

							if(cViewKind.startsWith("0")){
								inputViewID = cViewID;
								inputView = ViewTableNameHash.get(cViewID);
								outputViewID = "-";
								outputView = "-";
							} else {
								cFilterOutViewID = cViewID;
								//inputViewID = "-";
								//inputView = "-";
								outputViewID = cViewID;
								outputView = ViewTableNameHash.get(cViewID);
							}

							// -----------------------------------------------
							// データ構造出力 （「分割」）
							// -----------------------------------------------
							outLineStr = seqNo++
									+ ","
									+ modifyTime
									+ ","
									+ parentDir				// 業務名称（親フォルダ名称）
									+ ","
									+ fileNameBody

									+ ","
									+ vFilterKind 			// ビューフィルタ区分
									+ ","
									+ vFilterKindName + "ViewInfo"		// ビューフィルタ区分名称：TODO 2011/04/01
									+ ","
									+ vFilterName 			// ビューフィルタ名称
									+ ","
									+ outputViewID 		// 転送先・ビューID
									+ ","
									+ outputView  // 転送先・Viewテーブル名称
									+ ","
									+ ","
									+ ","
									+ ","
									+ inputViewID
									+ ","
									+ inputView
									+ "," // Viewテーブル名称
									+ ","
									+ ","
									+ ","
									+ ","
									+ ","
									+ ","
									+ ","
									+ ","
									+ ","
									+ ","
									+ ","
									+ ","
									;

							// ファイル出力
							System.out.println(outLineStr);
							PW.println(outLineStr);
					}
					break;

				case 12:	//「参照」 20161004
					// [LookupViewInfo] ブロック処理
					blockLineCount += 1;
					// データレコード時（3行目以降）の処理
					if (blockLineCount > 2 ) {
							// レコード行の分割（配列）
							String[] columnsList = line.split("\t");
							// 参照ファイル
							cViewID = columnsList[0].trim(); 	// 参照ビューID
							String outputViewID = "-";
							String outputView = "参照" ;
							inputViewID = cViewID;
							inputView = ViewTableNameHash.get(cViewID);

							// -----------------------------------------------
							// データ構造出力 （「参照」）
							// ※TODO: [ViewInfo]時、出力先を大域変数に入れておけば、グラフ情報となる。
							// -----------------------------------------------
							outLineStr = seqNo++
									+ ","
									+ modifyTime
									+ ","
									+ parentDir				// 業務名称（親フォルダ名称）
									+ ","
									+ fileNameBody

									+ ","
									+ vFilterKind 			// ビューフィルタ区分
									+ ","
									+ vFilterKindName + "LookupViewInfo"		// ビューフィルタ区分名称：TODO 2011/04/01
									+ ","
									+ vFilterName 			// ビューフィルタ名称
									+ ","
									+ outputViewID 		// 転送先・ビューID
									+ ","
									+ outputView  // 転送先・Viewテーブル名称
									+ ","
									+ ","
									+ ","
									+ ","
									+ inputViewID
									+ ","
									+ inputView
									+ "," // Viewテーブル名称
									+ ","
									+ ","
									+ ","
									+ ","
									+ ","
									+ ","
									+ ","
									+ ","
									+ ","
									+ ","
									+ ","
									+ ","
									;

							// ファイル出力
							System.out.println(">>" +outLineStr);
							PW.println(outLineStr);
					}
					break;



				default:
					break;
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			try {
				lnr.close(); // WTJファイル・ストロームのクローズ
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
	}

	/**
	 * 配列中から、Key に一致するIndex を返す。
	 *
	 * @param keyList
	 *            探査対象配列
	 * @param key
	 *            探査キーワード
	 */
	private static int getKeysindex(String[] keyList, String key) {
		for (int i = 0; i < keyList.length; i++) {
			if (keyList[i].matches(key)) {
				return i;
			}
		}
		// マッチした項目が存在しない。
		return -1;
	}

	/**
	 * ファイル名から拡張子を返す。
	 *
	 * @param fileName
	 *            ファイル名
	 *
	 * @return ファイルの拡張子
	 */
	public static String getSuffix(String fileName) {
		if (fileName == null)
			return null;
		int point = fileName.lastIndexOf(".");
		if (point != -1) {
			return fileName.substring(point + 1);
		}
		return fileName;
	}

	/**
	 * ファイル名からファイル名「ボディ部」を返す。
	 *
	 * @param fileName
	 *            ファイル名
	 * @return ファイル「ボディ部」
	 */
	public static String getFileNameBody(String fileName) {
		if (fileName == null)
			return null;
		int point = fileName.lastIndexOf("\\");
		if (point != -1) {
			return fileName.substring(point + 1);
		}
		return fileName;
	}

	/**
	 * フルパスからカレントディレクトリ（業務名称）を取得する。
	 *
	 * @param filePath ファイルパス
	 * @return カレントディレクトリ名称
	 * 2014/04/19
	 */
	public static String getParentDir(String filePath) {
		if (filePath == null)
			return null;
		String sCurrentDir ;

		try{
            File abspath = new File(filePath).getAbsoluteFile();

            if(abspath.exists()){
            	//カレントディレクトリの取得
            	//sCurrentDir = new File("filePath").getAbsoluteFile().getParent();
				sCurrentDir = abspath.getParent();
            } else {
            	return "-";
            }

		} catch (Exception e) {
			return "-";
		}

		return getFileNameBody(sCurrentDir);
	}

	/**
	 * 数値文字列（正整数）から数値を返す。
	 *
	 * @param str
	 *            数値文字列
	 *
	 * @return 数値
	 */
	public static int string2Number(String str) {
		if (str == null)
			return 0;
		int n = 0;
		try {
			n = Integer.parseInt(str);
		} catch (Exception e) {
			return -1;
		}
		return n;
	}

	/**
	 * 現在日付(yyyymmdd)を返す。
	 *
	 * @return yyyyMMdd
	 */
	public static String getDateStamp() {
		Date date1 = new Date();
		SimpleDateFormat sdf1 = new SimpleDateFormat("yyyy'年'MM'月'dd'日'");

		sdf1.applyPattern("yyyyMMdd");
		//sdf1.applyPattern("yyyyMMddHHmmss");

		return sdf1.format(date1);
	}

	/**
	 * 解析結果ファイルのパスを返す。
	 *
	 */
	public String getWritePath() {
		return WRITE_PATH;
	}

	/**
	 * メッセージを返す。
	 *
	 */
	public String getMessage() {
		return msg;
	}

	/**
	 * Waha関数名返す。
	 *
	 * ※ハッシュに存在しない関数IDの場合、空文字を返す。
	 *
	 */
	public static String getFunctionName(int n) {
		String fname = functionNameHash.get(n);

		if (fname != null) {
			return fname;
		} else {
			return "";
		}
	}

	/**
	 * ビューフィルタ種別名称を返す。
	 *
	 * ※ハッシュに存在しない関数IDの場合、空文字を返す。
	 *
	 */
	public static String getViewFilterName(int n) {
		String fname = viewFilterNameHash.get(n);

		if (fname != null) {
			return fname;
		} else {
			return " ";
		}
	}

	/**
	 * 二項比較子・記号名称を返す。
	 *
	 * ※ハッシュに存在しない関数IDの場合、空文字を返す。
	 *
	 */
	public static String getOperatorName(String num) {
		assert num != null;

		String opename = compOpeNameHash.get(string2Number(num));

		if (opename != null) {
			return opename;
		} else {
			return " ";
		}
	}

	/**
	 * Waha関数、ビューフィルタ種別名、二項演算子のリストをハッシュマップに設定
	 *
	 */
	private void initFunctionList() {
		//Waha関数ハッシュ
		for (int i = 0; i < common.FUNCTION.length; i++) {
			int functionID = string2Number(common.FUNCTION[i][0]);
			String functionName = common.FUNCTION[i][1];

			functionNameHash.put(functionID, functionName);
		}

		//ビューフィルタ・ハッシュ
		for (int i = 0; i < common.VIEWFITLERNAME.length; i++) {
			int ViewFilterID = string2Number(common.VIEWFITLERNAME[i][0]);
			String ViewFilterName = common.VIEWFITLERNAME[i][1];

			viewFilterNameHash.put(ViewFilterID, ViewFilterName);
		}

		//二項比較子・ハッシュ
		for (int i = 0; i < common.COMPOPENAME.length; i++) {
			int ViewFilterID = string2Number(common.COMPOPENAME[i][0]);
			String OpeName = common.COMPOPENAME[i][1];

			compOpeNameHash.put(ViewFilterID, OpeName);
		}
	}

	/**
	 * ファイルパスからファイルの「タイムスタンプ」を取得しこれを返す。
	 *
	 * @param path ファイルパス
	 * @return ファイルのタイムスタンプ("yyyyMMddhhmmss")
	 * @date 2013-04-18
	 */
	private static String getFileLastModifyTime(String path) {
		File file = new File(path);
		long lastModifytime = file.lastModified();
		Date date = new Date(lastModifytime);
		//System.out.println(date.toString());
		//DateFormat format = new SimpleDateFormat("yyyyMMddhhmmss");
		DateFormat format = new SimpleDateFormat("yyyyMMddHHmmss");

		return format.format(date);
	}

}

