*** Constant Group: CursorTypeEnum 
#define adOpenUnspecified                                 -1
#define adOpenForwardOnly                                 0
#define adOpenKeyset                                      1
#define adOpenDynamic                                     2
#define adOpenStatic                                      3

*** Constant Group: CursorOptionEnum
#define adHoldRecords                                     256
#define adMovePrevious                                    512
#define adAddNew                                          16778240
#define adDelete                                          16779264
#define adUpdate                                          16809984
#define adBookmark                                        8192
#define adApproxPosition                                  16384
#define adUpdateBatch                                     65536
#define adResync                                          131072
#define adNotify                                          262144
#define adFind                                            524288
#define adSeek                                            4194304
#define adIndex                                           8388608

*** Constant Group: LockTypeEnum
#define adLockUnspecified                                 -1
#define adLockReadOnly                                    1
#define adLockPessimistic                                 2
#define adLockOptimistic                                  3
#define adLockBatchOptimistic                             4

*** Constant Group: ExecuteOptionEnum
#define adOptionUnspecified                               -1
#define adAsyncExecute                                    16
#define adAsyncFetch                                      32
#define adAsyncFetchNonBlocking                           64
#define adExecuteNoRecords                                128

*** Constant Group: ConnectOptionEnum
#define adConnectUnspecified                              -1
#define adAsyncConnect                                    16

*** Constant Group: ObjectStateEnum
#define adStateClosed                                     0
#define adStateOpen                                       1
#define adStateConnecting                                 2
#define adStateExecuting                                  4
#define adStateFetching                                   8

*** Constant Group: CursorLocationEnum
#define adUseNone                                         1
#define adUseServer                                       2
#define adUseClient                                       3
#define adUseClientBatch                                  3

*** Constant Group: DataTypeEnum
#define adEmpty                                           0
#define adTinyInt                                         16
#define adSmallInt                                        2
#define adInteger                                         3
#define adBigInt                                          20
#define adUnsignedTinyInt                                 17
#define adUnsignedSmallInt                                18
#define adUnsignedInt                                     19
#define adUnsignedBigInt                                  21
#define adSingle                                          4
#define adDouble                                          5
#define adCurrency                                        6
#define adDecimal                                         14
#define adNumeric                                         131
#define adBoolean                                         11
#define adError                                           10
#define adUserDefined                                     132
#define adVariant                                         12
#define adIDispatch                                       9
#define adIUnknown                                        13
#define adGUID                                            72
#define adDate                                            7
#define adDBDate                                          133
#define adDBTime                                          134
#define adDBTimeStamp                                     135
#define adBSTR                                            8
#define adChar                                            129
#define adVarChar                                         200
#define adLongVarChar                                     201
#define adWChar                                           130
#define adVarWChar                                        202
#define adLongVarWChar                                    203
#define adBinary                                          128
#define adVarBinary                                       204
#define adLongVarBinary                                   205
#define adChapter                                         136
#define adFileTime                                        64
#define adDBFileTime                                      137
#define adPropVariant                                     138
#define adVarNumeric                                      139

*** Constant Group: FieldAttributeEnum
#define adFldUnspecified                                  -1
#define adFldMayDefer                                     2
#define adFldUpdatable                                    4
#define adFldUnknownUpdatable                             8
#define adFldFixed                                        16
#define adFldIsNullable                                   32
#define adFldMayBeNull                                    64
#define adFldLong                                         128
#define adFldRowID                                        256
#define adFldRowVersion                                   512
#define adFldCacheDeferred                                4096
#define adFldNegativeScale                                16384
#define adFldKeyColumn                                    32768

*** Constant Group: EditModeEnum
#define adEditNone                                        0
#define adEditInProgress                                  1
#define adEditAdd                                         2
#define adEditDelete                                      4

*** Constant Group: RecordStatusEnum
#define adRecOK                                           0
#define adRecNew                                          1
#define adRecModified                                     2
#define adRecDeleted                                      4
#define adRecUnmodified                                   8
#define adRecInvalid                                      16
#define adRecMultipleChanges                              64
#define adRecPendingChanges                               128
#define adRecCanceled                                     256
#define adRecCantRelease                                  1024
#define adRecConcurrencyViolation                         2048
#define adRecIntegrityViolation                           4096
#define adRecMaxChangesExceeded                           8192
#define adRecObjectOpen                                   16384
#define adRecOutOfMemory                                  32768
#define adRecPermissionDenied                             65536
#define adRecSchemaViolation                              131072
#define adRecDBDeleted                                    262144

*** Constant Group: GetRowsOptionEnum
#define adGetRowsRest                                     -1

*** Constant Group: PositionEnum
#define adPosUnknown                                      -1
#define adPosBOF                                          -2
#define adPosEOF                                          -3

*** Constant Group: BookmarkEnum
#define adBookmarkCurrent                                 0
#define adBookmarkFirst                                   1
#define adBookmarkLast                                    2

*** Constant Group: MarshalOptionsEnum
#define adMarshalAll                                      0
#define adMarshalModifiedOnly                             1

*** Constant Group: AffectEnum
#define adAffectCurrent                                   1
#define adAffectGroup                                     2
#define adAffectAll                                       3
#define adAffectAllChapters                               4

*** Constant Group: ResyncEnum
#define adResyncUnderlyingValues                          1
#define adResyncAllValues                                 2

*** Constant Group: CompareEnum
#define adCompareLessThan                                 0
#define adCompareEqual                                    1
#define adCompareGreaterThan                              2
#define adCompareNotEqual                                 3
#define adCompareNotComparable                            4

*** Constant Group: FilterGroupEnum
#define adFilterNone                                      0
#define adFilterPendingRecords                            1
#define adFilterAffectedRecords                           2
#define adFilterFetchedRecords                            3
#define adFilterPredicate                                 4
#define adFilterConflictingRecords                        5

*** Constant Group: SearchDirectionEnum
#define adSearchForward                                   1
#define adSearchBackward                                  -1

*** Constant Group: SearchDirection
#define adSearchForward                                   1
#define adSearchBackward                                  -1

*** Constant Group: PersistFormatEnum
#define adPersistADTG                                     0
#define adPersistXML                                      1

*** Constant Group: StringFormatEnum
#define adClipString                                      2

*** Constant Group: ConnectPromptEnum
#define adPromptAlways                                    1
#define adPromptComplete                                  2
#define adPromptCompleteRequired                          3
#define adPromptNever                                     4

*** Constant Group: ConnectModeEnum
#define adModeUnknown                                     0
#define adModeRead                                        1
#define adModeWrite                                       2
#define adModeReadWrite                                   3
#define adModeShareDenyRead                               4
#define adModeShareDenyWrite                              8
#define adModeShareExclusive                              12
#define adModeShareDenyNone                               16

*** Constant Group: IsolationLevelEnum
#define adXactUnspecified                                 -1
#define adXactChaos                                       16
#define adXactReadUncommitted                             256
#define adXactBrowse                                      256
#define adXactCursorStability                             4096
#define adXactReadCommitted                               4096
#define adXactRepeatableRead                              65536
#define adXactSerializable                                1048576
#define adXactIsolated                                    1048576

*** Constant Group: XactAttributeEnum
#define adXactCommitRetaining                             131072
#define adXactAbortRetaining                              262144
#define adXactAsyncPhaseOne                               524288
#define adXactSyncPhaseOne                                1048576

*** Constant Group: PropertyAttributesEnum
#define adPropNotSupported                                0
#define adPropRequired                                    1
#define adPropOptional                                    2
#define adPropRead                                        512
#define adPropWrite                                       1024

*** Constant Group: ErrorValueEnum
#define adErrInvalidArgument                              3001
#define adErrNoCurrentRecord                              3021
#define adErrIllegalOperation                             3219
#define adErrInTransaction                                3246
#define adErrFeatureNotAvailable                          3251
#define adErrItemNotFound                                 3265
#define adErrObjectInCollection                           3367
#define adErrObjectNotSet                                 3420
#define adErrDataConversion                               3421
#define adErrObjectClosed                                 3704
#define adErrObjectOpen                                   3705
#define adErrProviderNotFound                             3706
#define adErrBoundToCommand                               3707
#define adErrInvalidParamInfo                             3708
#define adErrInvalidConnection                            3709
#define adErrNotReentrant                                 3710
#define adErrStillExecuting                               3711
#define adErrOperationCancelled                           3712
#define adErrStillConnecting                              3713
#define adErrNotExecuting                                 3715
#define adErrUnsafeOperation                              3716

*** Constant Group: ParameterAttributesEnum
#define adParamSigned                                     16
#define adParamNullable                                   64
#define adParamLong                                       128

*** Constant Group: ParameterDirectionEnum
#define adParamUnknown                                    0
#define adParamInput                                      1
#define adParamOutput                                     2
#define adParamInputOutput                                3
#define adParamReturnValue                                4

*** Constant Group: CommandTypeEnum
#define adCmdUnspecified                                  -1
#define adCmdUnknown                                      8
#define adCmdText                                         1
#define adCmdTable                                        2
#define adCmdStoredProc                                   4
#define adCmdFile                                         256
#define adCmdTableDirect                                  512

*** Constant Group: EventStatusEnum
#define adStatusOK                                        1
#define adStatusErrorsOccurred                            2
#define adStatusCantDeny                                  3
#define adStatusCancel                                    4
#define adStatusUnwantedEvent                             5

*** Constant Group: EventReasonEnum
#define adRsnAddNew                                       1
#define adRsnDelete                                       2
#define adRsnUpdate                                       3
#define adRsnUndoUpdate                                   4
#define adRsnUndoAddNew                                   5
#define adRsnUndoDelete                                   6
#define adRsnRequery                                      7
#define adRsnResynch                                      8
#define adRsnClose                                        9
#define adRsnMove                                         10
#define adRsnFirstChange                                  11
#define adRsnMoveFirst                                    12
#define adRsnMoveNext                                     13
#define adRsnMovePrevious                                 14
#define adRsnMoveLast                                     15

*** Constant Group: SchemaEnum
#define adSchemaProviderSpecific                          -1
#define adSchemaAsserts                                   0
#define adSchemaCatalogs                                  1
#define adSchemaCharacterSets                             2
#define adSchemaCollations                                3
#define adSchemaColumns                                   4
#define adSchemaCheckConstraints                          5
#define adSchemaConstraintColumnUsage                     6
#define adSchemaConstraintTableUsage                      7
#define adSchemaKeyColumnUsage                            8
#define adSchemaReferentialContraints                     9
#define adSchemaReferentialConstraints                    9
#define adSchemaTableConstraints                          10
#define adSchemaColumnsDomainUsage                        11
#define adSchemaIndexes                                   12
#define adSchemaColumnPrivileges                          13
#define adSchemaTablePrivileges                           14
#define adSchemaUsagePrivileges                           15
#define adSchemaProcedures                                16
#define adSchemaSchemata                                  17
#define adSchemaSQLLanguages                              18
#define adSchemaStatistics                                19
#define adSchemaTables                                    20
#define adSchemaTranslations                              21
#define adSchemaProviderTypes                             22
#define adSchemaViews                                     23
#define adSchemaViewColumnUsage                           24
#define adSchemaViewTableUsage                            25
#define adSchemaProcedureParameters                       26
#define adSchemaForeignKeys                               27
#define adSchemaPrimaryKeys                               28
#define adSchemaProcedureColumns                          29
#define adSchemaDBInfoKeywords                            30
#define adSchemaDBInfoLiterals                            31
#define adSchemaCubes                                     32
#define adSchemaDimensions                                33
#define adSchemaHierarchies                               34
#define adSchemaLevels                                    35
#define adSchemaMeasures                                  36
#define adSchemaProperties                                37
#define adSchemaMembers                                   38
#define adSchemaTrustees                                  39

*** Constant Group: SeekEnum
#define adSeekFirstEQ                                     1
#define adSeekLastEQ                                      2
#define adSeekAfterEQ                                     4
#define adSeekAfter                                       8
#define adSeekBeforeEQ                                    16
#define adSeekBefore                                      32

*** Constant Group: ADCPROP_UPDATECRITERIA_ENUM
#define adCriteriaKey                                     0
#define adCriteriaAllCols                                 1
#define adCriteriaUpdCols                                 2
#define adCriteriaTimeStamp                               3

*** Constant Group: ADCPROP_ASYNCTHREADPRIORITY_ENUM
#define adPriorityLowest                                  1
#define adPriorityBelowNormal                             2
#define adPriorityNormal                                  3
#define adPriorityAboveNormal                             4
#define adPriorityHighest                                 5

*** Constant Group: CEResyncEnum
#define adResyncNone                                      0
#define adResyncAutoIncrement                             1
#define adResyncConflicts                                 2
#define adResyncUpdates                                   4
#define adResyncInserts                                   8
#define adResyncAll                                       15

*** Constant Group: ADCPROP_AUTORECALC_ENUM
#define adRecalcUpFront                                   0
#define adRecalcAlways                                    1
