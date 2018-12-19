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
 * �uWTJ�v���O�����E��̓c�[���v
 *
 *  ���ݒ�l����",(comma)"�A�Ȃ�тɁA""(double quatation)"�́A�o�̓t�@�C����
 *  �@�����Ă��ꂼ��A"�A"�A"�h"�ɕϊ�����B
 *
 * �s�o�̓t�H�[�}�b�g (20130604)�t
 *  1 �V�[�P���X�ԍ�	�@���l������	15		����	���o�̓V�[�P���X(20130604 �V�K�ǉ�)
 *  2 �^�C���X�^���v	�@���l������	15		����	��20130418 �V�K�ǉ�
 *  3 WTJ�t�@�C����	�@�@�@�@�@�@����	128		����	WTJ�t�@�C������
 *  4 �r���[�t�B���^���	�@�@���l	2	0	����	0�F�W��, 1�F����, 2�F����, 4�F, 5�F�Q��
 *  5 �r���[�t�B���^��ʖ���	����	8		����
 *  6 �r���[�t�B���^����	�@�@����	99		����	ViewFilter����
 *  7 �ڑ���r���[ID	�@���l������	3	0	����	�ڑ���E�r���[ID
 *  8 �ڑ���r���[�e�[�u������	����	99		����
 *  9 �ڑ���J����ID	�@���l������	4	0	����	�ڑ���E�J����ID
 * 10 �ڑ���J������	�@���l������	4	0	����
 * 11 �ڑ���J��������	�@�@�@�@����	99		����	�ڑ���E�J��������
 * 12 �ڑ����r���[ID	�@���l������	3	0	����	�ڑ����E�r���[ID
 * 13 �ڑ����r���[�e�[�u������	����	99		����
 * 14 �ڑ����J����ID	�@���l������	5	0	����	�ڑ����E�J����ID
 * 15 �ڑ����J������	�@���l������	4	0	����
 * 16 �ڑ����J��������	�@�@�@�@����	256		����	�ڑ����E�J��������
 * 17 �ڑ��l�^	�@�@�@�@�@���l������	3	0	����	Val�^�C�v�t���O
 * 18 �ڑ��l	�@�@�@�@�@�@�@�@����	256		����	���l(Value)
 * 19 �֐��^�C�v	�@�@�@���l������	4	0	����	�֐�ID
 * 20 ���ڈڑ��t���O	�@���l������	1	0	����	�P�F���ڈڑ�
 * 21 ���o�����i�l�j	�@�@�@�@����	32		����
 * 22 ���o�����i���Z�q�j	�@�@����	8		����
 * 23 ���o�����i���ځj	�@�@�@�@����	64		����
 *
 *
 * @author hiroyuki-c
 * @date 2011/04/01
 * Update 2013/04/19 �o�͍��ڒǉ��i�i�[�t�H���_�i�Ɩ����j�j
 * Update 2015/04/06 wtj�̃o�[�W�����`�F�b�N���W�b�N�ǉ��iRepository�j
 * Update 2016/09/23 wtj�̃o�[�W�����`�F�b�N���W�b�N�ǉ��iProcess)
 * Update 2016/10/04 wtj�̃o�[�W�����`�F�b�N���W�b�N�ǉ��iViewInfo(DivideInfo):�u�����v�u�����v�u�W�v�v)
 * Update 2016/10/04 wtj�̃o�[�W�����`�F�b�N���W�b�N�ǉ��iLookupViewInfo:�u�Q�Ɓv)
 * Update 2016/10/07 �o�̓t�@�C�����ύX�i"yyyyMMddHHmmss"��"yyyyMMdd"�jcf.getDateStamp()
 *
 */
public class WTJAnalyze {
	// �o�̓t�@�C��
	private static String WRITE_PATH = "";
	private static PrintWriter PW = null;

	// ���O�t�@�C��
	private static String LOG_PATH = "";
	private static PrintWriter PW_LOG = null;

	//Message
	private static String msg = null;

	// �S���R�[�h�J�E���^�[�i�V�[�P���X�ԍ��j
	private static long seqNo = 1;

	// �^�C�g���s
	private static String TITLE = "SeqNo,"
			+ "�^�C���X�^���v,"
			+ "�i�[�t�H���_�i�Ɩ����j,"
			+ "WTJ�t�@�C����,"
			+ "�r���[�t�B���^���,"
			+ "�r���[�t�B���^��ʖ���,"
			+ "�r���[�t�B���^����,"
			+ "�ڑ���r���[ID,"
			+ "�ڑ���r���[�e�[�u������,"
			+ "�ڑ���J����ID,"
			+ "�ڑ���J������,"
			+ "�ڑ���J��������,"
			+ "�ڑ����r���[ID,"
			+ "�ڑ����r���[�e�[�u������,"
			+ "�ڑ����J����ID,"
			+ "�ڑ����J������,"
			+ "�ڑ����J��������,"
			+ "�ڑ��l�^,"
			+ "�ڑ��l,"
			+ "�֐��^�C�v,"
			+ "�����t���O,"
			+ "���o�����i�����l�j,"
			+ "���o�����i��r���Z�q�j,"
			+ "���o�����i���ځj,"
			+ "WorkDir,"
			+ "�����i0:�O�^1:��),"
			+ "CommandLine";

	// �n�b�V���F<Waha�֐�ID>,<�֐�����>
	private static Hashtable<Integer, String> functionNameHash = new Hashtable<Integer, String>();
	// �n�b�V���F<�r���[�t�B���^ID>,<�r���[�t�B���^����>
	private static Hashtable<Integer, String> viewFilterNameHash = new Hashtable<Integer, String>();
	// �n�b�V���F<�񍀔�r�qID>,<�L������>
	private static Hashtable<Integer, String> compOpeNameHash = new Hashtable<Integer, String>();

	//20120529 �uView�v�F�e�[�u����`���̂��i�[����B��ID���C���f�b�N�X�Ƃ����X�g�����O���̂��i�[����B
	//private static String viewTable[] ;
	//private static String[] viewTable ;
	// �n�b�V���F<�񍀔�r�qID>,<�L������>
	private static Hashtable<String, String> ViewTableNameHash = new Hashtable<String, String>();

	public WTJAnalyze(String path) {
		try {
			//Waha�֐����̎擾�p�n�b�V���̏�����
			initFunctionList();

			seqNo = 1;	//20161006

			// �o�̓t�@�C������
			WRITE_PATH = ".\\WTJStructureData_" + getDateStamp() + ".csv";
			PW = new PrintWriter(new BufferedWriter(new FileWriter(WRITE_PATH)));
			PW.println(TITLE);

			// ���O�t�@�C������
			LOG_PATH = ".\\WTJStructureData_" + getDateStamp() + ".log";
			PW_LOG = new PrintWriter(new BufferedWriter(new FileWriter(LOG_PATH)));

			/*
			 *  WTJ�f�[�^��͍ċA����
			 */
			File dir = new File(path);

			if(dir.isDirectory()) {
				//�ċA����
				ListPath(dir);
			} else {
				msg = "[Error]:�w�肳�ꂽ�f�B���N�g���p�X�͑��݂��܂���B";
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
		// ��͊J�n�t�H���_�i�f�t�H���g�Fjar�N���t�H���_�j
		// -----------------------------------------------
		String path = ".\\";

		//��͊J�n
		WTJAnalyze runner = new WTJAnalyze(path);
	}

	/**
	 * �t�H���_�p�X���󂯁A�w��t�H���_�����ċA��������B
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
						// WTJ�f�[�^�\�����͊J�n
						Analyze(f.getCanonicalPath());
				} catch (IOException ex) {
					// do nothing
				}
			}
		}
	}

	/**
	 * �w��̃t�@�C����Ǎ��݁AWTJ�f�[�^�̃p�[�X�������s���B
	 *
	 * @param target_file_path Analyze
	 */
	private static void Analyze(String target_file_path) {
		String READ_PATH = target_file_path;
		String modifyTime = ""; //�^�C���X�^���v
		String parentDir = ""; //�i�[�t�H���_���́i�Ɩ����j

		// �g���q(wtj)�ȊO�͏������Ȃ��B
		if (getSuffix(READ_PATH).matches("wtj")) {
			// continue
		} else {
			// �����ΏۊO�t�@�C��
			return;
		}
		String fileNameBody = getFileNameBody(READ_PATH);

		// ���O�t�@�C���ɏo��
		PW_LOG.println("��READ_PATH=" + READ_PATH);

		// �t�@�C��READER������
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
		//20161004 �Q��
		Pattern pat_LookupViewInfo = Pattern.compile("^\\[LookupViewInfo\\]");

		try {
			// �t�@�C���̃^�C���X�^���v
			modifyTime = getFileLastModifyTime(READ_PATH);

			// �i�[�t�H���_���́i�Ɩ����jadd.20130419
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

			// �������ۑ��p�n�b�V���e�[�u��
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

			// [Prosess] �R�}���h���C��
			String cDicision = null;
			String cWorkDir = null;
			String cCommandLine = null;

			// [ViewInfo] �R�}���h���C��
			String cViewKind = null;
			//String cCommandLine.
			String cViewID= null;
			String inputViewID = null;
			String inputView = null;

			// �\���X�e�[�^�X
			int status = 0;
			// �u���b�N���E�s�J�E���^�[
			int blockLineCount = 0;
			// �Ǎ��ݍs
			String line = null;
			while ((line = lnr.readLine()) != null) {
				//seqNo++; //�V�[�P���X�ԍ�
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

				//20141222 add Waha �o�[�W����
				} else if (pat_Repository.matcher(line).matches()) {
					// [Repository]
					status = 9;
					blockLineCount = 0;

				//20160923 add Waha �o�[�W����
				} else if (pat_Process.matcher(line).matches()) {
					// [Repository]
					status = 10;
					blockLineCount = 0;

				//20161004 �u�����v�u�W���v�u�W�v�v[ViewInfo]
				} else if (pat_ViewInfo.matcher(line).matches()) {
					// [ViewInfo]
					status = 11;
					blockLineCount = 0;

				//20161004 �u�Q�Ɓv[LookupViewInfo]
				} else if (pat_LookupViewInfo.matcher(line).matches()) {
					// [LookupViewInfo]
					status = 12;
					blockLineCount = 0;  //TODO ?
					//System.out.println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>12");

				} else if (pat_Otherx.matcher(x).matches()) {
					//�����ΏۊO
					status = 999;
					blockLineCount = 0;
				}

				// �t�@�C���o�͕����񏉊���
				String outLineStr = null;

				switch (status) {
				case 1:
					// [View] �u���b�N����
					blockLineCount += 1;
					// �f�[�^���R�[�h���i3�s�ځj�̏���
					if (blockLineCount == 3) {
						String[] columnsList = line.split("\t");
						viewID = columnsList[7].trim();

						//20120529 �e�[�u�����́iFileName)���n�b�V���i�[
						//int vid = (int)Integer.valueOf(viewID);
						//String vn = new String(columnsList[5].trim());
						//viewTable[vid] = vn;
						//ViewTableNameHash.put(viewID , columnsList[5].trim());
						ViewTableNameHash.put(viewID , columnsList[8].trim());
					}
					break;

				case 2:
					// [Column]�u���b�N����
					blockLineCount += 1;
					// �f�[�^���R�[�h���i3�s�ڈȏ�j�̏���
					if (blockLineCount >= 3) {
						String[] columnsList = line.split("\t");
						// ���ږ���
						columnName = columnsList[5].trim();
						// �J����ID
						columnID = columnsList[10].trim();
						// Length
						columnLength = columnsList[6].trim();
						// ViewID��ColumnID���L�[�Ƃ����ږ��̂��n�b�V���ɓ����
						columnHash.put(viewID + "_" + columnID, columnName);
						columnHashLength.put(viewID + "_" + columnID, columnLength);
					}
					break;

				case 3:
					// [VFilter] �u���b�N����
					blockLineCount += 1;
					// �f�[�^���R�[�h���i2�s�ځj�̏���
					if (blockLineCount == 2) {
						// �^�C�g�����R�[�h���瑮�����̔z��("Kind","ID","ID")���쐬�B���u������B
						vFilterAttrList = line.split("\t");
					} else if (blockLineCount == 3) {
						// �f�[�^���R�[�h���i3�s�ځj�̏���
						String[] columnsList = line.split("\t");
						vFilterKind = columnsList[getKeysindex(vFilterAttrList,
								"Kind")];
						//�r���[�t�B���^���̂��n�b�V�����擾
						vFilterKindName =getViewFilterName(string2Number(vFilterKind));
						vFilterID = columnsList[getKeysindex(vFilterAttrList, "ID")];
						vFilterName = columnsList[getKeysindex(vFilterAttrList, "Name")];
					}
					break;

				case 4:
					// [CFilter] �u���b�N����
					blockLineCount += 1;
					// �f�[�^���R�[�h���i3�s�ځj�̏���
					if (blockLineCount == 3) {
						String[] columnsList = line.split("\t");
						// �ڑ���E����ID�̎擾
						cFilterOutViewID = columnsList[0].trim();
						cFilterOutColID = columnsList[1].trim();
						// cFilterFileName = columnsList[8].trim();
						// cFilterName = columnsList[5].trim();
					}
					break;

				case 5:
					// [CFValue] �u���b�N����
					blockLineCount += 1;
					// �f�[�^���R�[�h���i3�s�ڈȍ~�j�̏���
					if (blockLineCount >= 3) {
						// ���R�[�h�s�̕����i�z��j
						String[] columnsList = line.split("\t");
						// �ڑ����E�������̎擾
						cFValueViewID = columnsList[0].trim(); 			// �ڑ����E�r���[ID
						cFValueValue = columnsList[2].trim(); 			// �ڑ����E�ڑ��l
						cFValueSFType = columnsList[4].trim(); 			// �֐��^�C�v
						cFValueRealValueType = columnsList[5].trim(); 	// ���l�^
						cFValuelikColID = columnsList[9].trim(); 		// �ڑ����E�J����ID
						cFValueID = columnsList[11].trim();
						cFValueIsRoot = columnsList[10].trim(); 		// ���[�g�t���O
						cFValueChildIDs = columnsList[13].trim(); 		// �p�����[�^
						//�񍀔�r�q
						cFValueValueType = columnsList[1].trim();    	//ValueType
						cFValueSetValueType = columnsList[3].trim(); 	//SetValueType
						cFValueObjType = columnsList[6].trim();      	//ObjType

						// �]�����E���ږ���
						if (cFValueViewID.equals("0")
								&& cFValuelikColID.equals("0")) {
							cFValueLength = "0";
							int functionNumber = string2Number(cFValueSFType);
							if (functionNumber > 0) {
								//Waha�֐��̐ݒ�
								String functionName = getFunctionName(functionNumber);
								if(functionName.length() > 0){
									//�֐������o�^���Ă���ꍇ
									cFValueName = "��Function[" + functionName + "]";
								} else {
									//�֐������o�^����Ă��Ȃ��ꍇ�A�֐�ID���o��
									cFValueName = "��Function[" + cFValueSFType + "]";
								}
							} else if (string2Number(cFValueRealValueType) > 0) {
								// �J���}��u���iCSV�o�̓t�@�C���ɉe�����邽�߁j
								cFValueValue = cFValueValue .replaceAll(",", "�A");
								// �_�u���N�I�[�e�[�V������u���iCSV�o�̓t�@�C���ɉe�����邽�߁j
								cFValueValue = cFValueValue .replaceAll("\"", "�h");

								cFValueName = "��Value[" + cFValueValue + "]";
							} else if (cFValueValueType.matches("6") &&  cFValueSetValueType.matches("101") && cFValueObjType.matches("5") 	) {
								// 1:��, 2:��, 3:��, 4:��, 5:��, 6:���Ԉ�v, 7:�O����v, 8:�����v
								String operatorName = getOperatorName(cFValueValue);
								cFValueValue = "";
								cFValueName = "��Ope[" + operatorName + "]";
							} else if (cFValueValueType.matches("1") &&  cFValueSetValueType.matches("8") && cFValueObjType.matches("4") 	) {
								// Null
								cFValueValue = "";
								cFValueName = "��Value[Null]";
							} else {
								// ��͕s�蕪�i���Z�b�g����[Numberring]�A�폜���j
								cFValueValue = "";
								cFValueName = "�����ݒ�";
							}
						} else {
							//�]�������擾
							cFValueName = columnHash.get(cFValueViewID + "_"
									+ cFValuelikColID);
							cFValueLength = columnHashLength.get(cFValueViewID
									+ "_" + cFValuelikColID);
						}

						// ���ڈڑ��̌��m �iIsRoot=1 ���AChilsIds=0�̏ꍇ�A"���ڈڑ�"�Ƃ݂Ȃ��j
						String directFlg = "0";
						if (cFValueIsRoot.equals("1")
								&& cFValueChildIDs.equals("0")) {
							directFlg = "1";
						}

						// -----------------------------------------------
						// �f�[�^�\���o��
						// -----------------------------------------------
						outLineStr = seqNo++
								+ ","
								+ modifyTime         // �^�C��s�^���v�i20130418)
								+ ","
								+ parentDir				// �Ɩ����́i�e�t�H���_���́j

								+ ","
								+ fileNameBody
								+ ","
								+ vFilterKind 			// �r���[�t�B���^�敪
								+ ","
								+ vFilterKindName 		// �r���[�t�B���^�敪���́FTODO 2011/04/01
								+ ","
								+ vFilterName 			// �r���[�t�B���^����
								+ ","
								+ cFilterOutViewID 		// �]����E�r���[ID
								+ ","

								//20120529
								+ ViewTableNameHash.get(cFilterOutViewID)  // �]����EView�e�[�u������

								+ ","
								+ cFilterOutColID 		// �]����E�J����ID
								+ ","
								+ columnHashLength.get(cFilterOutViewID + "_"
										+ cFilterOutColID) 		// �]����E���ڒ�
								+ ","
								+ columnHash.get(cFilterOutViewID + "_" + cFilterOutColID) + "," // �]����E���ږ���
								+ cFValueViewID + "," 	// �]�����E�r���[ID

								//20120529
								+ ViewTableNameHash.get(cFValueViewID) + "," // �]�����EView�e�[�u������

								+ cFValuelikColID + "," // �]�����E�J����ID
								+ cFValueLength + "," 	// �]�����E�J������
								+ cFValueName + "," 	// �]�����E�J��������

								+ cFValueRealValueType + "," 	// ���l�^
								+ "\"" + cFValueValue + "\"," 	// �ݒ�l
								+ cFValueSFType + "," 	// �֐��^�C�v
								+ directFlg; 			// ���ڈڑ��t���O�i1�F���ڈڑ��j
						// �t�@�C���o��
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
					// [Select][DViewSelectCondition][DeleteSelectCondition] �u���b�N����
					blockLineCount += 1;
					// �f�[�^���R�[�h���i3�s�ڈȍ~�j�̏���
					if (blockLineCount >= 3) {
							// ���R�[�h�s�̕����i�z��j
							String[] columnsList = line.split("\t");
							// �ڑ����E�������̎擾
							cSelectConditionViewID = columnsList[0].trim(); 		// �Q�ƃr���[ID
							cSelectConditionValue = columnsList[2].trim(); 			// �ݒ�l
							cSelectConditionOperator = columnsList[3].trim(); 		// �I�y���[�^�[
							cSelectConditionColumnID = columnsList[10].trim(); 		// �Q�ƃe�[�u���E�J����ID

							// 1:��, 2:��, 3:��, 4:��, 5:��, 6:���Ԉ�v, 7:�O����v, 8:�����v
							String operatorName = getOperatorName(cSelectConditionOperator);
							// -----------------------------------------------
							// �f�[�^�\���o�� �i���o�����j
							// -----------------------------------------------
							outLineStr = seqNo++
									+ ","
									+ modifyTime

									+ ","
									+ parentDir				// �Ɩ����́i�e�t�H���_���́j

									+ ","
									+ fileNameBody
									+ ","
									+ ","
									+ "���o����,"
									+ vFilterName 			// �r���[�t�B���^����
									+ ","
									+ ","
									+ ","
									+ ","
									+ ","
									+ ","
									+ ","
									+ ViewTableNameHash.get(cSelectConditionViewID) + "," // View�e�[�u������
									+ ","
									+ ","
									+ columnHash.get(cSelectConditionViewID + "_" + cSelectConditionColumnID)  // ��������
									+ ","
									+ ","
									+ ","
									+ ","
									+ ","
									+ cSelectConditionValue 		// ���o�����l�A���ϐ�
									+ ","
									+ operatorName					// �I�y���[�^�[
									+ ","
									+ columnHash.get(cSelectConditionViewID + "_" + cSelectConditionColumnID)  // ��������
									;

							// �t�@�C���o��
							System.out.println(outLineStr);
							PW.println(outLineStr);
					}
					break;

				case 9:
					// [RepositoryVersion] �u���b�N����
					blockLineCount += 1;
					// �f�[�^���R�[�h���i3�s�ځj�̏���
					if (blockLineCount == 3) {
						String[] columnsList = line.split("\t");
						// "�o�[�W����" �̎擾
						String repositoryVersion;
						repositoryVersion = columnsList[1].trim();
						if(repositoryVersion.startsWith("2") == false) {
							//msg = "\n\r[WahaVersionCheckError]: WTJ�t�@�C���́AWaha [" + repositoryVersion + " ] �ł��B�o�[�W�����ΏۊO�̂��ߐ�������͂��ł��܂���B\n\r" + READ_PATH + "\n\r";
							msg = "\r[WahaVersionCheckError]: WTJ�t�@�C���́AWaha [" + repositoryVersion + " ] �ł��B�o�[�W�����ΏۊO�̂��ߐ�������͂��ł��܂���B\r" + READ_PATH + "\r";
							System.out.println(msg);
							PW_LOG.println(msg);
							PW.println(msg);
							msg = "\n[WahaVersionCheckError]: Waha�o�[�W�����ΏۊO�� WTJ�t�@�C�� ������܂��B���O���m�F���ĉ������B\n\r";
						}
					}
					break;

				case 10:
					// [Process] �u���b�N���� / 2016.09.23
					blockLineCount += 1;
					// �f�[�^���R�[�h���i3�s�ڈȍ~�j�̏���
					if (blockLineCount > 2 ) {
							// ���R�[�h�s�̕����i�z��j
							String[] columnsList = line.split("\t");
							// �ڑ����E�������̎擾
							cWorkDir = columnsList[0].trim(); 		// WorkDir
							cDicision = columnsList[11].trim(); 		// �O����[0]�A�㏈��[1]
							cCommandLine = columnsList[12].trim(); 		// CommandLine

							// -----------------------------------------------
							// �f�[�^�\���o�� �i�R�}���h���C��������j
							// -----------------------------------------------
							outLineStr = seqNo++
									+ ","
									+ modifyTime
									+ ","
									+ parentDir				// �Ɩ����́i�e�t�H���_���́j
									+ ","
									+ fileNameBody

									+ ","
									+ vFilterKind 			// �r���[�t�B���^�敪
									+ ","
									+ vFilterKindName 		// �r���[�t�B���^�敪���́FTODO 2011/04/01
									+ ","
									+ vFilterName 			// �r���[�t�B���^����
									+ ","
									+ cFilterOutViewID 		// �]����E�r���[ID
									+ ","
									+ ViewTableNameHash.get(cFilterOutViewID)  // �]����EView�e�[�u������
									+ ","
									+ ","
									+ ","
									+ ","
									+ ","
									+ "," // View�e�[�u������
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
							+ "CMDLINE��" + cCommandLine
							;

							// �t�@�C���o��
							System.out.println(outLineStr);
							PW.println(outLineStr);
					}
					break;

				case 11:	//�u�����v�u�W���v 20161004
					// �W���F���̓t�@�C���@�P�A�o�̓t�@�C���@�P
					// �����F���̓t�@�C���@�P�A�o�̓t�@�C���@����
					// [ViewInfo] �u���b�N����
					blockLineCount += 1;
					// �f�[�^���R�[�h���i3�s�ڈȍ~�j�̏���
					if (blockLineCount > 2 ) {
							// ���R�[�h�s�̕����i�z��j
							String[] columnsList = line.split("\t");
							// ���̓t�@�C���^�����t�@�C�����̂̎擾
							cViewKind = columnsList[0].trim(); 		// 0:���̓t�@�C���A1:�����t�@�C��
							cViewID = columnsList[1].trim(); 		// �O����[0]�A�㏈��[1]

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
							// �f�[�^�\���o�� �i�u�����v�j
							// -----------------------------------------------
							outLineStr = seqNo++
									+ ","
									+ modifyTime
									+ ","
									+ parentDir				// �Ɩ����́i�e�t�H���_���́j
									+ ","
									+ fileNameBody

									+ ","
									+ vFilterKind 			// �r���[�t�B���^�敪
									+ ","
									+ vFilterKindName + "ViewInfo"		// �r���[�t�B���^�敪���́FTODO 2011/04/01
									+ ","
									+ vFilterName 			// �r���[�t�B���^����
									+ ","
									+ outputViewID 		// �]����E�r���[ID
									+ ","
									+ outputView  // �]����EView�e�[�u������
									+ ","
									+ ","
									+ ","
									+ ","
									+ inputViewID
									+ ","
									+ inputView
									+ "," // View�e�[�u������
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

							// �t�@�C���o��
							System.out.println(outLineStr);
							PW.println(outLineStr);
					}
					break;

				case 12:	//�u�Q�Ɓv 20161004
					// [LookupViewInfo] �u���b�N����
					blockLineCount += 1;
					// �f�[�^���R�[�h���i3�s�ڈȍ~�j�̏���
					if (blockLineCount > 2 ) {
							// ���R�[�h�s�̕����i�z��j
							String[] columnsList = line.split("\t");
							// �Q�ƃt�@�C��
							cViewID = columnsList[0].trim(); 	// �Q�ƃr���[ID
							String outputViewID = "-";
							String outputView = "�Q��" ;
							inputViewID = cViewID;
							inputView = ViewTableNameHash.get(cViewID);

							// -----------------------------------------------
							// �f�[�^�\���o�� �i�u�Q�Ɓv�j
							// ��TODO: [ViewInfo]���A�o�͐����ϐ��ɓ���Ă����΁A�O���t���ƂȂ�B
							// -----------------------------------------------
							outLineStr = seqNo++
									+ ","
									+ modifyTime
									+ ","
									+ parentDir				// �Ɩ����́i�e�t�H���_���́j
									+ ","
									+ fileNameBody

									+ ","
									+ vFilterKind 			// �r���[�t�B���^�敪
									+ ","
									+ vFilterKindName + "LookupViewInfo"		// �r���[�t�B���^�敪���́FTODO 2011/04/01
									+ ","
									+ vFilterName 			// �r���[�t�B���^����
									+ ","
									+ outputViewID 		// �]����E�r���[ID
									+ ","
									+ outputView  // �]����EView�e�[�u������
									+ ","
									+ ","
									+ ","
									+ ","
									+ inputViewID
									+ ","
									+ inputView
									+ "," // View�e�[�u������
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

							// �t�@�C���o��
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
				lnr.close(); // WTJ�t�@�C���E�X�g���[���̃N���[�Y
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
	}

	/**
	 * �z�񒆂���AKey �Ɉ�v����Index ��Ԃ��B
	 *
	 * @param keyList
	 *            �T���Ώ۔z��
	 * @param key
	 *            �T���L�[���[�h
	 */
	private static int getKeysindex(String[] keyList, String key) {
		for (int i = 0; i < keyList.length; i++) {
			if (keyList[i].matches(key)) {
				return i;
			}
		}
		// �}�b�`�������ڂ����݂��Ȃ��B
		return -1;
	}

	/**
	 * �t�@�C��������g���q��Ԃ��B
	 *
	 * @param fileName
	 *            �t�@�C����
	 *
	 * @return �t�@�C���̊g���q
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
	 * �t�@�C��������t�@�C�����u�{�f�B���v��Ԃ��B
	 *
	 * @param fileName
	 *            �t�@�C����
	 * @return �t�@�C���u�{�f�B���v
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
	 * �t���p�X����J�����g�f�B���N�g���i�Ɩ����́j���擾����B
	 *
	 * @param filePath �t�@�C���p�X
	 * @return �J�����g�f�B���N�g������
	 * 2014/04/19
	 */
	public static String getParentDir(String filePath) {
		if (filePath == null)
			return null;
		String sCurrentDir ;

		try{
            File abspath = new File(filePath).getAbsoluteFile();

            if(abspath.exists()){
            	//�J�����g�f�B���N�g���̎擾
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
	 * ���l������i�������j���琔�l��Ԃ��B
	 *
	 * @param str
	 *            ���l������
	 *
	 * @return ���l
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
	 * ���ݓ��t(yyyymmdd)��Ԃ��B
	 *
	 * @return yyyyMMdd
	 */
	public static String getDateStamp() {
		Date date1 = new Date();
		SimpleDateFormat sdf1 = new SimpleDateFormat("yyyy'�N'MM'��'dd'��'");

		sdf1.applyPattern("yyyyMMdd");
		//sdf1.applyPattern("yyyyMMddHHmmss");

		return sdf1.format(date1);
	}

	/**
	 * ��͌��ʃt�@�C���̃p�X��Ԃ��B
	 *
	 */
	public String getWritePath() {
		return WRITE_PATH;
	}

	/**
	 * ���b�Z�[�W��Ԃ��B
	 *
	 */
	public String getMessage() {
		return msg;
	}

	/**
	 * Waha�֐����Ԃ��B
	 *
	 * ���n�b�V���ɑ��݂��Ȃ��֐�ID�̏ꍇ�A�󕶎���Ԃ��B
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
	 * �r���[�t�B���^��ʖ��̂�Ԃ��B
	 *
	 * ���n�b�V���ɑ��݂��Ȃ��֐�ID�̏ꍇ�A�󕶎���Ԃ��B
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
	 * �񍀔�r�q�E�L�����̂�Ԃ��B
	 *
	 * ���n�b�V���ɑ��݂��Ȃ��֐�ID�̏ꍇ�A�󕶎���Ԃ��B
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
	 * Waha�֐��A�r���[�t�B���^��ʖ��A�񍀉��Z�q�̃��X�g���n�b�V���}�b�v�ɐݒ�
	 *
	 */
	private void initFunctionList() {
		//Waha�֐��n�b�V��
		for (int i = 0; i < common.FUNCTION.length; i++) {
			int functionID = string2Number(common.FUNCTION[i][0]);
			String functionName = common.FUNCTION[i][1];

			functionNameHash.put(functionID, functionName);
		}

		//�r���[�t�B���^�E�n�b�V��
		for (int i = 0; i < common.VIEWFITLERNAME.length; i++) {
			int ViewFilterID = string2Number(common.VIEWFITLERNAME[i][0]);
			String ViewFilterName = common.VIEWFITLERNAME[i][1];

			viewFilterNameHash.put(ViewFilterID, ViewFilterName);
		}

		//�񍀔�r�q�E�n�b�V��
		for (int i = 0; i < common.COMPOPENAME.length; i++) {
			int ViewFilterID = string2Number(common.COMPOPENAME[i][0]);
			String OpeName = common.COMPOPENAME[i][1];

			compOpeNameHash.put(ViewFilterID, OpeName);
		}
	}

	/**
	 * �t�@�C���p�X����t�@�C���́u�^�C���X�^���v�v���擾�������Ԃ��B
	 *
	 * @param path �t�@�C���p�X
	 * @return �t�@�C���̃^�C���X�^���v("yyyyMMddhhmmss")
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

