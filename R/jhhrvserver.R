#' Title 预览数据
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples viewserver()
viewserver <- function(input, output, session, dms_token) {
  var_file_export_baseInfo = tsui::var_file('btn_hrv_voucher_uploadfile')
  var_hr_sheet = tsui::var_ListChoose1('btn_hrv_voucher_sheet')
  
  shiny::observe({
    shiny::observeEvent(input$btn_hrv_voucher_view,
                        {
                          # 获取文件路径
                          file_name = var_file_export_baseInfo()
                          print(file_name)
                          
                          # 获取表名
                          sheet_name = var_hr_sheet()
                          print(sheet_name)
                          
                          if (sheet_name == '工资') {
                            print(sheet_name)
                            salary_data_excel <-
                              readxl::read_excel(
                                file_name,
                                sheet = '工资',
                                col_types = c(
                                  "text",
                                  "text",
                                  "text",
                                  "text",
                                  # "text",
                                  "text",
                                  "numeric",
                                  "numeric",
                                  "numeric",
                                  "numeric",
                                  "numeric",
                                  "numeric",
                                  "numeric",
                                  "numeric",
                                  "numeric",
                                  "numeric",
                                  "text",
                                  "text",
                                  "text",
                                  "numeric",
                                  "date",
                                  "text"
                                )
                              )
                            salary_data_excel = as.data.frame(salary_data_excel)
                            
                            salary_data_excel = tsdo::na_standard(salary_data_excel)
                            
                            #显示数据
                            tsui::run_dataTable2(id = 'btn_hrv_voucher_view_data', data = salary_data_excel)
                            
                            
                          }
                          else if (sheet_name == '社保') {
                            #读取文件
                            socialsecurity_data_excel <-
                              readxl::read_excel(
                                file_name,
                                sheet = '社保',
                                col_types = c(
                                  "text",
                                  "text",
                                  "text",
                                  # "text",
                                  "text",
                                  "text",
                                  "numeric",
                                  "numeric",
                                  "numeric",
                                  "numeric",
                                  "numeric",
                                  "numeric",
                                  "numeric",
                                  "numeric",
                                  "numeric",
                                  "numeric",
                                  "numeric",
                                  "numeric",
                                  "numeric",
                                  "numeric",
                                  "numeric",
                                  "numeric",
                                  "numeric",
                                  "numeric",
                                  "numeric",
                                  "numeric",
                                  "numeric",
                                  "numeric",
                                  "numeric",
                                  "numeric",
                                  "text",
                                  "text",
                                  "text",
                                  "numeric",
                                  "date",
                                  "text"
                                )
                              )
                            
                            socialsecurity_data_excel = as.data.frame(socialsecurity_data_excel)
                            socialsecurity_data_excel = tsdo::na_standard(socialsecurity_data_excel)
                            
                            #显示数据
                            tsui::run_dataTable2(id = 'btn_hrv_voucher_view_data', data = socialsecurity_data_excel)
                            
                            
                          }
                          else if (sheet_name == '工时') {
                            #读取文件
                            redetail <-
                              readxl::read_excel(file_name, sheet = '工时')
                            
                            ncols = ncol(redetail)
                            c1 = c("numeric", 
                                   "text", "numeric", "numeric", "text", 
                                   "text", "text", "text", "text", "numeric", 
                                   "numeric", "numeric", "text")
                            
                            c2<-rep("numeric",times= (ncols - 13))
                            col_types = c(c1, c2)
                            
                            redetail <- readxl::read_excel(file_name, 
                                                 sheet = "工时", col_types = col_types)
                            
                            
                            redetail = as.data.frame(redetail)
                            redetail = tsdo::na_standard(redetail)
                            
                            #显示数据
                            tsui::run_dataTable2(id = 'btn_hrv_voucher_view_data', data = redetail)
                            print(redetail)
                            
                          }
                          else {
                            print('请查看上传数据的模版，或联系棱星顾问')
                          }
                          
                        })
    
  })
}


#' Title 上传数据
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples uploadserver()
uploadserver <- function(input, output, session, dms_token) {
  var_file_export_baseInfo = tsui::var_file('btn_hrv_voucher_uploadfile')
  var_hr_sheet = tsui::var_ListChoose1('btn_hrv_voucher_sheet')
  
  shiny::observeEvent(input$btn_hrv_voucher_upload,
                      {
                        dsql = 'truncate table rds_hrv_src_ds_salary_input'
                        tsda::sql_update2(token = dms_token, sql_str = dsql)
                        
                        dsql = 'truncate table rds_hrv_src_ds_socialsecurity_input'
                        tsda::sql_update2(token = dms_token, sql_str = dsql)
                        
                        dsql = 'truncate table rds_hrv_src_ds_rddetail_input'
                        tsda::sql_update2(token = dms_token, sql_str = dsql)
                        
                        dsql = 'truncate table rds_hrv_src_ds_nonrddetail_input'
                        tsda::sql_update2(token = dms_token, sql_str = dsql)
                        
                        
                        # 获取文件路径
                        file_name = var_file_export_baseInfo()
                        print(file_name)
                        
                        salary_data_excel <-
                          readxl::read_excel(
                            file_name,
                            sheet = '工资',
                            col_types = c(
                              "text",
                              "text",
                              "text",
                              "text",
                              # "text",
                              "text",
                              "numeric",
                              "numeric",
                              "numeric",
                              "numeric",
                              "numeric",
                              "numeric",
                              "numeric",
                              "numeric",
                              "numeric",
                              "numeric",
                              "text",
                              "text",
                              "text",
                              "numeric",
                              "date",
                              "text"
                            )
                          )
                        
                        
                        
                        names(salary_data_excel) = c(
                          'FExpenseOrgID',
                          'FTaxDeclarationOrg',
                          'FBankType',
                          'FAccount',
                          # 'FHightechDept',
                          'FRdProject',
                          'FCpayAmount',
                          'FFixdCost',
                          'FScraprateCost',
                          'FSocialSecurityAmt',
                          'FAccumulationFundAmt',
                          'FOtherAmt',
                          'FIncomeTaxAmt',
                          'FActualAmount',
                          'FYear',
                          'FMonth',
                          'FVoucher',
                          'FCategoryType',
                          'FNumber',
                          'FSeq',
                          'FDate',
                          'FOldDept'
                        )
                        
                        FHightechDept = ''
                        salary_data_excel = cbind(salary_data_excel[, 1:4], FHightechDept, salary_data_excel[, 5:ncol(salary_data_excel)])
                        
                        salary_data_excel = as.data.frame(salary_data_excel)
                        salary_data_excel = tsdo::na_standard(salary_data_excel)
                        
                        # 写入中间表
                        tsda::db_writeTable2(
                          token = dms_token,
                          table_name = 'rds_hrv_src_ds_salary_input',
                          r_object = salary_data_excel,
                          append = FALSE
                        )
                        
                        sql = 'insert into rds_hrv_src_ds_salary_history select * from rds_hrv_src_ds_salary  where FNumber in (select FNumber from rds_hrv_src_ds_salary_input)'
                        tsda::sql_insert2(token = dms_token, sql_str = sql)
                        
                        dsql = 'delete a from rds_hrv_src_ds_salary  a inner join rds_hrv_src_ds_salary_input b On a.FNumber=b.FNumber and a.FYear =b.FYear and a.FMonth =b.FMonth'
                        tsda::sql_update2(token = dms_token, sql_str = dsql)
                        
                        dsql = 'delete a from rds_hrv_ods_ds_salary  a inner join rds_hrv_src_ds_salary_input b On a.FNumber=b.FNumber and a.FYear =b.FYear and a.FMonth =b.FMonth'
                        tsda::sql_update2(token = dms_token, sql_str = dsql)
                        
                        dsql = 'delete a from rds_hrv_std_ds_salary  a inner join rds_hrv_src_ds_salary_input b On a.FNumber=b.FNumber and a.FYear =b.FYear and a.FMonth =b.FMonth'
                        tsda::sql_update2(token = dms_token, sql_str = dsql)
                        
                        isql = 'insert into rds_hrv_src_ds_salary  select * from rds_hrv_src_ds_salary_input'
                        tsda::sql_insert2(token = dms_token, sql_str = isql)
                        
                        # dsql = 'truncate table rds_hrv_src_ds_salary_input'
                        # tsda::sql_update2(token = dms_token, sql_str = dsql)
                        
                        
                        socialsecurity_data_excel <-
                          readxl::read_excel(
                            file_name,
                            sheet = '社保',
                            col_types = c(
                              "text",
                              "text",
                              "text",
                              # "text",
                              "text",
                              "text",
                              "numeric",
                              "numeric",
                              "numeric",
                              "numeric",
                              "numeric",
                              "numeric",
                              "numeric",
                              "numeric",
                              "numeric",
                              "numeric",
                              "numeric",
                              "numeric",
                              "numeric",
                              "numeric",
                              "numeric",
                              "numeric",
                              "numeric",
                              "numeric",
                              "numeric",
                              "numeric",
                              "numeric",
                              "numeric",
                              "numeric",
                              "numeric",
                              "text",
                              "text",
                              "text",
                              "numeric",
                              "date",
                              "text"
                            )
                          )
                        names(socialsecurity_data_excel) = c(
                          'FExpenseOrgID',
                          'FTaxDeclarationOrg',
                          'FBankType',
                          # 'FHightechDept',
                          'FAccount',
                          'FRdProject',
                          'FComPensionBenefitsAmt',
                          'FComMedicareAmt',
                          'FComMedicareOfSeriousAmt',
                          'FComDisabilityBenefitsAmt',
                          'FComOffsiteElseAmt',
                          'FComWorklessInsuranceAmt',
                          'FComInjuryInsuranceAmt',
                          'FComMaternityInsuranceAmt',
                          'FComAllSocialSecurityAmt',
                          'FComAccumulationFundAmt',
                          'FComAllSoSeAcFuAmt',
                          'FEmpPensionBenefitsAmt',
                          'FEmpMedicareAmt',
                          'FEmpMedicareOfSeriousAmt',
                          'FEmpWorklessInsuranceAmt',
                          'FEmpAllSocialSecurityAmt',
                          'FEmpAccumulationFundAmt',
                          'FEmpAllSoSeAcFuAmt',
                          'FAllSocialSecurityAmt',
                          'FAllAccumulationFundAmt',
                          'FAllAmount',
                          'FManagementAmount',
                          'FYear',
                          'FMonth',
                          'FVoucher',
                          'FCategoryType',
                          'FNumber',
                          'FSeq',
                          'FDate',
                          'FOldDept'
                        )
                        
                        # socialsecurity_data_excel$FHightechDept = ''
                        
                        FHightechDept = ''
                        socialsecurity_data_excel = cbind(socialsecurity_data_excel[, 1:3],
                                                          FHightechDept,
                                                          socialsecurity_data_excel[, 4:ncol(socialsecurity_data_excel)])
                        
                        socialsecurity_data_excel = as.data.frame(socialsecurity_data_excel)
                        socialsecurity_data_excel = tsdo::na_standard(socialsecurity_data_excel)
                        
                        
                        # 写入社保公积金中间表
                        tsda::db_writeTable2(
                          token = dms_token,
                          table_name = 'rds_hrv_src_ds_socialsecurity_input',
                          r_object = socialsecurity_data_excel,
                          append = FALSE
                        )
                        
                        sql = 'insert into rds_hrv_src_ds_socialsecurity_history select * from rds_hrv_src_ds_socialsecurity  where FNumber in (select FNumber from rds_hrv_src_ds_socialsecurity_input)'
                        tsda::sql_insert2(token = dms_token, sql_str = sql)
                        
                        dsql = 'delete a from rds_hrv_src_ds_socialsecurity  a inner join rds_hrv_src_ds_socialsecurity_input b On a.FNumber=b.FNumber and a.FYear =b.FYear and a.FMonth =b.FMonth'
                        tsda::sql_update2(token = dms_token, sql_str = dsql)
                        
                        dsql = 'delete a from rds_hrv_ods_ds_socialsecurity  a inner join rds_hrv_src_ds_socialsecurity_input b On a.FNumber=b.FNumber and a.FYear =b.FYear and a.FMonth =b.FMonth'
                        tsda::sql_update2(token = dms_token, sql_str = dsql)

                        dsql = 'delete a from rds_hrv_std_ds_socialsecurity  a inner join rds_hrv_src_ds_socialsecurity_input b On a.FNumber=b.FNumber and a.FYear =b.FYear and a.FMonth =b.FMonth'
                        tsda::sql_update2(token = dms_token, sql_str = dsql)
                        
                        isql = 'insert into rds_hrv_src_ds_socialsecurity  select * from rds_hrv_src_ds_socialsecurity_input'
                        tsda::sql_insert2(token = dms_token, sql_str = isql)
                        
                        # dsql = 'truncate table rds_hrv_src_ds_socialsecurity_input'
                        # tsda::sql_update2(token = dms_token, sql_str = dsql)
                        
                        
                        
                        #读取文件
                        redetail <-
                          readxl::read_excel(file_name, sheet = '工时')
                        
                        ncols = ncol(redetail)
                        c1 = c("numeric", 
                               "text", "numeric", "numeric", "text", 
                               "text", "text", "text", "text", "numeric", 
                               "numeric", "numeric", "text")
                        
                        c2<-rep("numeric",times= (ncols - 13))
                        col_types = c(c1, c2)
                        
                        redetail <- readxl::read_excel(file_name, 
                                               sheet = "工时", col_types = col_types)
                        
                        redetail = as.data.frame(redetail)
                        redetail = tsdo::na_standard(redetail)
                        
                        # 将非研发金额空值替换为0
                        # redetail$非研发工资成本          = tsdo::na_replace(redetail$非研发工资成本, 0)
                        
                        # 非研发工时表字段
                        col_nonrd = c(
                          '序号',
                          '工资类别',
                          '会计年度',
                          '会计期间',
                          '原部门',
                          '高新部门',
                          '姓名',
                          '费用承担组织',
                          '个税申报组织',
                          '单据编号',
                          '非研发工资成本'
                        )
                        
                        # 筛选非研发金额不为0的数据
                        nonrddetail = redetail[redetail$非研发工资成本           != 0, col_nonrd]
                        
                        # 更名为数据库字段名
                        names(nonrddetail) = c(
                          'FNO',
                          'FSalaryType',
                          'FYear',
                          'FMonth',
                          'FOldDept',
                          'FHightechDept',
                          'FStaffName',
                          'FExpenseOrgID',
                          'FTaxDeclarationOrg',
                          'FNumber',
                          'FNonRdCost'
                        )
                        
                        
                        if (nrow(nonrddetail) > 0) {
                          nonrddetail$FRdProject = ''
                          
                          # 写入非研发工时中间表
                          tsda::db_writeTable2(
                            token = dms_token,
                            table_name = 'rds_hrv_src_ds_nonrddetail_input',
                            r_object = nonrddetail,
                            append = FALSE
                          )
                          
                          sql = 'insert into rds_hrv_src_ds_nonrddetail_history select * from rds_hrv_src_ds_nonrddetail  where FNumber in (select FNumber from rds_hrv_src_ds_nonrddetail_input)'
                          tsda::sql_insert2(token = dms_token, sql_str = sql)
                          
                          dsql = 'delete a from rds_hrv_src_ds_nonrddetail  a inner join rds_hrv_src_ds_nonrddetail_input b On a.FNumber=b.FNumber and a.FYear =b.FYear and a.FMonth =b.FMonth'
                          tsda::sql_update2(token = dms_token, sql_str = dsql)
                          
                          dsql = 'delete a from rds_hrv_ods_ds_nonrddetail  a inner join rds_hrv_src_ds_nonrddetail_input b On a.FNumber=b.FNumber and a.FYear =b.FYear and a.FMonth =b.FMonth'
                          tsda::sql_update2(token = dms_token, sql_str = dsql)
                          
                          isql = 'insert into rds_hrv_src_ds_nonrddetail  select * from rds_hrv_src_ds_nonrddetail_input'
                          tsda::sql_insert2(token = dms_token, sql_str = isql)
                          
                          # dsql = 'truncate table rds_hrv_src_ds_nonrddetail_input'
                          # tsda::sql_update2(token = dms_token, sql_str = dsql)
                        }
                        
                        
                        # 工时表固定字段
                        col_fixed = c(
                          '序号',
                          '工资类别',
                          '会计年度',
                          '会计期间',
                          '原部门',
                          '高新部门',
                          '姓名',
                          '费用承担组织',
                          '个税申报组织',
                          '单据编号',
                          '单项总额',
                          '研发工资成本',
                          '非研发工资成本'
                        )
                        
                        # 研发项目列转行
                        redetail2 <-
                          reshape2::melt(
                            data = redetail,
                            id.vars = col_fixed,
                            variable.name = '研发项目',
                            value.name = '研发金额'
                          )
                        
                        # 研发工时表字段
                        col_rd = c(
                          '序号',
                          '工资类别',
                          '会计年度',
                          '会计期间',
                          '原部门',
                          '高新部门',
                          '姓名',
                          '费用承担组织',
                          '个税申报组织',
                          '单据编号',
                          '研发项目',
                          '研发金额'
                        )
                        
                        # 筛选研发工时需要字段
                        rddetail = redetail2[, col_rd]
                        
                        # 研发金额为空时替换为0
                        # rddetail$研发金额   = tsdo::na_replace(rddetail$研发金额, 0)
                        
                        # 筛选研发金额不为0数据
                        rddetail = rddetail[rddetail$研发金额           != 0, col_rd]
                        
                        # 替换为数据库字段
                        names(rddetail) = c(
                          'FNO',
                          'FSalaryType',
                          'FYear',
                          'FMonth',
                          'FOldDept',
                          'FHightechDept',
                          'FStaffName',
                          'FExpenseOrgID',
                          'FTaxDeclarationOrg',
                          'FNumber',
                          'FRdProject',
                          'FRdProjectCost'
                        )
                        
                        
                        
                        # 写入研发工时中间表
                        tsda::db_writeTable2(
                          token = dms_token,
                          table_name = 'rds_hrv_src_ds_rddetail_input',
                          r_object = rddetail,
                          append = FALSE
                        )
                        
                        sql = 'insert into rds_hrv_src_ds_rddetail_history select * from rds_hrv_src_ds_rddetail  where FNumber in (select FNumber from rds_hrv_src_ds_rddetail_input)'
                        tsda::sql_insert2(token = dms_token, sql_str = sql)
                        
                        dsql = 'delete a from rds_hrv_src_ds_rddetail  a inner join rds_hrv_src_ds_rddetail_input b On a.FNumber=b.FNumber and a.FYear =b.FYear and a.FMonth =b.FMonth'
                        tsda::sql_update2(token = dms_token, sql_str = dsql)
                        
                        dsql = 'delete a from rds_hrv_ods_ds_rddetail  a inner join rds_hrv_src_ds_rddetail_input b On a.FNumber=b.FNumber and a.FYear =b.FYear and a.FMonth =b.FMonth'
                        tsda::sql_update2(token = dms_token, sql_str = dsql)
                        
                        isql = 'insert into rds_hrv_src_ds_rddetail  select * from rds_hrv_src_ds_rddetail_input'
                        tsda::sql_insert2(token = dms_token, sql_str = isql)
                        
                        # dsql = 'truncate table rds_hrv_src_ds_rddetail_input'
                        # tsda::sql_update2(token = dms_token, sql_str = dsql)
                        
                        
                        tsui::pop_notice('数据上传成功')
                        
                        
                      })
}


#' Title 重新上传数据
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples reuploadserver()
# reuploadserver <- function(input, output, session, dms_token) {
#   var_file_export_baseInfo = tsui::var_file('uploadfile')
#   var_hr_sheet = tsui::var_ListChoose1('hr_sheet')
#   
#   shiny::observeEvent(input$btn_reupload,
#                       {
#                         dsql = 'truncate table rds_hrv_src_ds_salary_input'
#                         tsda::sql_update2(token = dms_token, sql_str = dsql)
#                         
#                         dsql = 'truncate table rds_hrv_src_ds_socialsecurity_input'
#                         tsda::sql_update2(token = dms_token, sql_str = dsql)
#                         
#                         # 获取文件路径
#                         file_name = var_file_export_baseInfo()
#                         print(file_name)
#                         
#                         salary_data_excel <-
#                           readxl::read_excel(
#                             file_name,
#                             sheet = '工资',
#                             col_types = c(
#                               "text",
#                               "text",
#                               "text",
#                               "text",
#                               # "text",
#                               "text",
#                               "numeric",
#                               "numeric",
#                               "numeric",
#                               "numeric",
#                               "numeric",
#                               "numeric",
#                               "numeric",
#                               "numeric",
#                               "numeric",
#                               "numeric",
#                               "text",
#                               "text",
#                               "text",
#                               "numeric",
#                               "date",
#                               "text"
#                             )
#                           )
#                         
#                         
#                         
#                         names(salary_data_excel) = c(
#                           'FExpenseOrgID',
#                           'FTaxDeclarationOrg',
#                           'FBankType',
#                           'FAccount',
#                           # 'FHightechDept',
#                           'FRdProject',
#                           'FCpayAmount',
#                           'FFixdCost',
#                           'FScraprateCost',
#                           'FSocialSecurityAmt',
#                           'FAccumulationFundAmt',
#                           'FOtherAmt',
#                           'FIncomeTaxAmt',
#                           'FActualAmount',
#                           'FYear',
#                           'FMonth',
#                           'FVoucher',
#                           'FCategoryType',
#                           'FNumber',
#                           'FSeq',
#                           'FDate',
#                           'FOldDept'
#                         )
#                         
#                         # salary_data_excel$FHightechDept = ''
#                         
#                         FHightechDept = ''
#                         salary_data_excel = cbind(salary_data_excel[, 1:4], FHightechDept, salary_data_excel[, 5:ncol(salary_data_excel)])
#                         
#                         salary_data_excel = as.data.frame(salary_data_excel)
#                         salary_data_excel = tsdo::na_standard(salary_data_excel)
#                         
#                         # 写入中间表
#                         tsda::db_writeTable2(
#                           token = dms_token,
#                           table_name = 'rds_hrv_src_ds_salary_input',
#                           r_object = salary_data_excel,
#                           append = FALSE
#                         )
#                         
#                         dsql = 'delete a from rds_hrv_src_ds_salary  a inner join rds_hrv_src_ds_salary_input b On a.FNumber=b.FNumber and a.FYear =b.FYear and a.FMonth =b.FMonth'
#                         tsda::sql_update2(token = dms_token, sql_str = dsql)
#                         
#                         dsql = 'delete a from rds_hrv_ods_ds_salary  a inner join rds_hrv_src_ds_salary_input b On a.FNumber=b.FNumber and a.FYear =b.FYear and a.FMonth =b.FMonth'
#                         tsda::sql_update2(token = dms_token, sql_str = dsql)
#                         
#                         dsql = 'delete a from rds_hrv_std_ds_salary  a inner join rds_hrv_src_ds_salary_input b On a.FNumber=b.FNumber and a.FYear =b.FYear and a.FMonth =b.FMonth'
#                         tsda::sql_update2(token = dms_token, sql_str = dsql)
#                         
#                         isql = 'insert into rds_hrv_src_ds_salary  select * from rds_hrv_src_ds_salary_input'
#                         tsda::sql_insert2(token = dms_token, sql_str = isql)
#                         
#                         # dsql = 'truncate table rds_hrv_src_ds_salary_input'
#                         # tsda::sql_update2(token = dms_token, sql_str = dsql)
#                         
#                         socialsecurity_data_excel <-
#                           readxl::read_excel(
#                             file_name,
#                             sheet = '社保',
#                             col_types = c(
#                               "text",
#                               "text",
#                               "text",
#                               # "text",
#                               "text",
#                               "text",
#                               "numeric",
#                               "numeric",
#                               "numeric",
#                               "numeric",
#                               "numeric",
#                               "numeric",
#                               "numeric",
#                               "numeric",
#                               "numeric",
#                               "numeric",
#                               "numeric",
#                               "numeric",
#                               "numeric",
#                               "numeric",
#                               "numeric",
#                               "numeric",
#                               "numeric",
#                               "numeric",
#                               "numeric",
#                               "numeric",
#                               "numeric",
#                               "numeric",
#                               "numeric",
#                               "numeric",
#                               "text",
#                               "text",
#                               "text",
#                               "numeric",
#                               "date",
#                               "text"
#                             )
#                           )
#                         names(socialsecurity_data_excel) = c(
#                           'FExpenseOrgID',
#                           'FTaxDeclarationOrg',
#                           'FBankType',
#                           # 'FHightechDept',
#                           'FAccount',
#                           'FRdProject',
#                           'FComPensionBenefitsAmt',
#                           'FComMedicareAmt',
#                           'FComMedicareOfSeriousAmt',
#                           'FComDisabilityBenefitsAmt',
#                           'FComOffsiteElseAmt',
#                           'FComWorklessInsuranceAmt',
#                           'FComInjuryInsuranceAmt',
#                           'FComMaternityInsuranceAmt',
#                           'FComAllSocialSecurityAmt',
#                           'FComAccumulationFundAmt',
#                           'FComAllSoSeAcFuAmt',
#                           'FEmpPensionBenefitsAmt',
#                           'FEmpMedicareAmt',
#                           'FEmpMedicareOfSeriousAmt',
#                           'FEmpWorklessInsuranceAmt',
#                           'FEmpAllSocialSecurityAmt',
#                           'FEmpAccumulationFundAmt',
#                           'FEmpAllSoSeAcFuAmt',
#                           'FAllSocialSecurityAmt',
#                           'FAllAccumulationFundAmt',
#                           'FAllAmount',
#                           'FManagementAmount',
#                           'FYear',
#                           'FMonth',
#                           'FVoucher',
#                           'FCategoryType',
#                           'FNumber',
#                           'FSeq',
#                           'FDate',
#                           'FOldDept'
#                         )
#                         
#                         # socialsecurity_data_excel$FHightechDept = ''
#                         
#                         FHightechDept = ''
#                         socialsecurity_data_excel = cbind(socialsecurity_data_excel[, 1:3],
#                                                           FHightechDept,
#                                                           socialsecurity_data_excel[, 4:ncol(socialsecurity_data_excel)])
#                         
#                         socialsecurity_data_excel = as.data.frame(socialsecurity_data_excel)
#                         socialsecurity_data_excel = tsdo::na_standard(socialsecurity_data_excel)
#                         
#                         
#                         # 写入社保公积金中间表
#                         tsda::db_writeTable2(
#                           token = dms_token,
#                           table_name = 'rds_hrv_src_ds_socialsecurity_input',
#                           r_object = socialsecurity_data_excel,
#                           append = FALSE
#                         )
#                         
#                         dsql = 'delete a from rds_hrv_src_ds_socialsecurity  a inner join rds_hrv_src_ds_socialsecurity_input b On a.FNumber=b.FNumber and a.FYear =b.FYear and a.FMonth =b.FMonth'
#                         tsda::sql_update2(token = dms_token, sql_str = dsql)
#                         
#                         dsql = 'delete a from rds_hrv_ods_ds_socialsecurity  a inner join rds_hrv_src_ds_socialsecurity_input b On a.FNumber=b.FNumber and a.FYear =b.FYear and a.FMonth =b.FMonth'
#                         tsda::sql_update2(token = dms_token, sql_str = dsql)
#                         
#                         dsql = 'delete a from rds_hrv_std_ds_socialsecurity  a inner join rds_hrv_src_ds_socialsecurity_input b On a.FNumber=b.FNumber and a.FYear =b.FYear and a.FMonth =b.FMonth'
#                         tsda::sql_update2(token = dms_token, sql_str = dsql)
#                         
#                         isql = 'insert into rds_hrv_src_ds_socialsecurity  select * from rds_hrv_src_ds_socialsecurity_input'
#                         tsda::sql_insert2(token = dms_token, sql_str = isql)
#                         
#                         # dsql = 'truncate table rds_hrv_src_ds_socialsecurity_input'
#                         # tsda::sql_update2(token = dms_token, sql_str = dsql)
#                         
#                         
#                         
#                         #读取文件
#                         redetail <-
#                           readxl::read_excel(file_name, sheet = '工时')
#                         redetail = as.data.frame(redetail)
#                         redetail = tsdo::na_standard(redetail)
#                         
#                         # 将非研发金额空值替换为0
#                         # redetail$非研发工资成本          = tsdo::na_replace(redetail$非研发工资成本, 0)
#                         
#                         # 非研发工时表字段
#                         col_nonrd = c(
#                           '序号',
#                           '工资类别',
#                           '会计年度',
#                           '会计期间',
#                           '原部门',
#                           '高新部门',
#                           '姓名',
#                           '费用承担组织',
#                           '个税申报组织',
#                           '单据编号',
#                           '非研发工资成本'
#                         )
#                         
#                         # 筛选非研发金额不为0的数据
#                         nonrddetail = redetail[redetail$非研发工资成本           != 0, col_nonrd]
#                         
#                         # 更名为数据库字段名
#                         names(nonrddetail) = c(
#                           'FNO',
#                           'FSalaryType',
#                           'FYear',
#                           'FMonth',
#                           'FOldDept',
#                           'FHightechDept',
#                           'FStaffName',
#                           'FExpenseOrgID',
#                           'FTaxDeclarationOrg',
#                           'FNumber',
#                           'FNonRdCost'
#                         )
#                         
#                         
#                         if (nrow(nonrddetail) > 0) {
#                           nonrddetail$FRdProject = ''
#                           
#                           # 写入非研发工时中间表
#                           tsda::db_writeTable2(
#                             token = dms_token,
#                             table_name = 'rds_hrv_src_ds_nonrddetail_input',
#                             r_object = nonrddetail,
#                             append = FALSE
#                           )
#                           
#                           dsql = 'delete a from rds_hrv_src_ds_nonrddetail  a inner join rds_hrv_src_ds_nonrddetail_input b On a.FNumber=b.FNumber and a.FYear =b.FYear and a.FMonth =b.FMonth'
#                           tsda::sql_update2(token = dms_token, sql_str = dsql)
#                           
#                           dsql = 'delete a from rds_hrv_ods_ds_nonrddetail  a inner join rds_hrv_src_ds_nonrddetail_input b On a.FNumber=b.FNumber and a.FYear =b.FYear and a.FMonth =b.FMonth'
#                           tsda::sql_update2(token = dms_token, sql_str = dsql)
#                           
#                           isql = 'insert into rds_hrv_src_ds_nonrddetail  select * from rds_hrv_src_ds_nonrddetail_input'
#                           tsda::sql_insert2(token = dms_token, sql_str = isql)
#                           
#                           dsql = 'truncate table rds_hrv_src_ds_nonrddetail_input'
#                           tsda::sql_update2(token = dms_token, sql_str = dsql)
#                         }
#                         
#                         
#                         
#                         
#                         
#                         
#                         
#                         # 工时表固定字段
#                         col_fixed = c(
#                           '序号',
#                           '工资类别',
#                           '会计年度',
#                           '会计期间',
#                           '原部门',
#                           '高新部门',
#                           '姓名',
#                           '费用承担组织',
#                           '个税申报组织',
#                           '单据编号',
#                           '单项总额',
#                           '研发工资成本',
#                           '非研发工资成本'
#                         )
#                         
#                         # 研发项目列转行
#                         redetail2 <-
#                           reshape2::melt(
#                             data = redetail,
#                             id.vars = col_fixed,
#                             variable.name = '研发项目',
#                             value.name = '研发金额'
#                           )
#                         
#                         # 研发工时表字段
#                         col_rd = c(
#                           '序号',
#                           '工资类别',
#                           '会计年度',
#                           '会计期间',
#                           '原部门',
#                           '高新部门',
#                           '姓名',
#                           '费用承担组织',
#                           '个税申报组织',
#                           '单据编号',
#                           '研发项目',
#                           '研发金额'
#                         )
#                         
#                         # 筛选研发工时需要字段
#                         rddetail = redetail2[, col_rd]
#                         
#                         # 研发金额为空时替换为0
#                         # rddetail$研发金额   = tsdo::na_replace(rddetail$研发金额, 0)
#                         
#                         # 筛选研发金额不为0数据
#                         rddetail = rddetail[rddetail$研发金额           != 0, col_rd]
#                         
#                         # 替换为数据库字段
#                         names(rddetail) = c(
#                           'FNO',
#                           'FSalaryType',
#                           'FYear',
#                           'FMonth',
#                           'FOldDept',
#                           'FHightechDept',
#                           'FStaffName',
#                           'FExpenseOrgID',
#                           'FTaxDeclarationOrg',
#                           'FNumber',
#                           'FRdProject',
#                           'FRdProjectCost'
#                         )
#                         
#                         
#                         
#                         # 写入研发工时中间表
#                         tsda::db_writeTable2(
#                           token = dms_token,
#                           table_name = 'rds_hrv_src_ds_rddetail_input',
#                           r_object = rddetail,
#                           append = FALSE
#                         )
#                         
#                         dsql = 'delete a from rds_hrv_src_ds_rddetail  a inner join rds_hrv_src_ds_rddetail_input b On a.FNumber=b.FNumber and a.FYear =b.FYear and a.FMonth =b.FMonth'
#                         tsda::sql_update2(token = dms_token, sql_str = dsql)
#                         
#                         dsql = 'delete a from rds_hrv_ods_ds_rddetail  a inner join rds_hrv_src_ds_rddetail_input b On a.FNumber=b.FNumber and a.FYear =b.FYear and a.FMonth =b.FMonth'
#                         tsda::sql_update2(token = dms_token, sql_str = dsql)
#                         
#                         isql = 'insert into rds_hrv_src_ds_rddetail  select * from rds_hrv_src_ds_rddetail_input'
#                         tsda::sql_insert2(token = dms_token, sql_str = isql)
#                         
#                         dsql = 'truncate table rds_hrv_src_ds_rddetail_input'
#                         tsda::sql_update2(token = dms_token, sql_str = dsql)
#                         
#                         
#                         tsui::pop_notice('数据上传成功')
#                         
#                         
#                         
#                       })
# }


#' Title 生成凭证
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples voucherserver()
voucherserver <- function(input, output, session, dms_token) {
  var_environment = tsui::var_ListChoose1('btn_hrv_voucher_environment')
  var_hr_year = tsui::var_numeric('btn_hrv_voucher_year')
  var_hr_month = tsui::var_numeric('btn_hrv_voucher_month')
  
  shiny::observeEvent(input$btn_hrv_voucher_outputvoucher,
                      {
                        var_environment = var_environment()
                        var_hr_year = var_hr_year()
                        var_hr_month = var_hr_month()
                        
                        jhhrvvoucherpkg::outputvourchermain(dms_token, var_hr_year, var_hr_month, var_environment)
                        tsui::pop_notice('凭证生成成功')
                        
                        
                        sql1 = sprintf(
                          "select * from rds_hrv_ods_ds_middleTable where fisdo=1 and fyear='%d.0' and fmonth='%d.0'",
                          var_hr_year,
                          var_hr_month
                        )
                        print(sql1)
                        data1 = tsda::sql_select2(dms_token, sql1)
                        
                        
                        sql2 = sprintf(
                          "select * from rds_hrv_ods_ds_middleTable where fisdo=2 and fyear='%d.0' and fmonth='%d.0'",
                          var_hr_year,
                          var_hr_month
                        )
                        print(sql2)
                        data2 = tsda::sql_select2(dms_token, sql2)
                        
                        res = list(`成功数据` = data1, `异常数据` = data2)
                        # print(res)
                        
                        
                        tsui::run_download_xlsx(id = 'btn_hrv_voucher_download',
                                                data = res ,
                                                filename = '凭证处理日志.xlsx')
                        
                      })
}


#' Title 重新生成凭证
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples revoucherserver()
# revoucherserver <- function(input, output, session, dms_token) {
#   var_environment = tsui::var_ListChoose1('environment')
#   var_hr_year = tsui::var_numeric('hr_year')
#   var_hr_month = tsui::var_numeric('hr_month')
#
#   shiny::observeEvent(input$outputvoucher2,
#                       {
#                         var_environment = var_environment()
#                         var_hr_year = var_hr_year()
#                         var_hr_month = var_hr_month()
#                         print(var_hr_year)
#                         print(class(var_hr_year))
#
#                         jhhrvvoucherpkg::reoutputvourchermain(dms_token, var_hr_year, var_hr_month, var_environment)
#                         tsui::pop_notice('凭证重新生成成功')
#
#                         sql1 = sprintf(
#                           "select * from rds_hrv_ods_ds_middleTable where fisdo=1 and fyear='%d.0' and fmonth='%d.0'",
#                           var_hr_year,
#                           var_hr_month
#                         )
#                         print(sql1)
#                         data1 = tsda::sql_select2(dms_token, sql1)
#                         # tsui::run_download_xlsx(id = 'btn_Asone_download1',data = data1 ,filename = '成功数据.xlsx')
#
#
#                         sql2 = sprintf(
#                           "select * from rds_hrv_ods_ds_middleTable where fisdo=2 and fyear='%d.0' and fmonth='%d.0'",
#                           var_hr_year,
#                           var_hr_month
#                         )
#                         print(sql2)
#                         data2 = tsda::sql_select2(dms_token, sql2)
#
#                         res = list(`成功数据` = data1, `异常数据` = data2)
#                         # print(res)
#
#
#                         tsui::run_download_xlsx(id = 'btn_res_download',
#                                                 data = res ,
#                                                 filename = '凭证处理日志.xlsx')
#
#                       })
# }



#' Title 生成预览
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples viewserver()
viewvoucher <- function(input, output, session, dms_token) {
  var_environment = tsui::var_ListChoose1('btn_hrv_voucher_environment')
  var_hr_year = tsui::var_numeric('btn_hrv_voucher_year')
  var_hr_month = tsui::var_numeric('btn_hrv_voucher_month')
  shiny::observe({
    shiny::observeEvent(input$btn_hrv_voucher_view_voucher,
                        {
                          var_environment = var_environment()
                          var_hr_year = var_hr_year()
                          var_hr_month = var_hr_month()
                          
                          jhhrvvoucherpkg::middlevourchermain(dms_token, var_hr_year, var_hr_month, var_environment)
                          
                          sql = sprintf(
                            "select FDate,FYear,FMonth,FBillNO,FSeq,FNumber,FName,FTaxDeclarationOrg,FExpenseOrgID,FCategoryType,FNotes,FAccountBookID,
  FDealingUnitName,FDealingUnitNumber,FSupplierName,FSupplierNumber,FAccountName,FHightechDept,FSubjectNumber,FSubjectName,
  FLexitemProperty,FDeptNumber,FDeptName,FRdProject,FProjectNumber,FWorkCenterNumber,FAcctreClassNumber,FBankAccount,
  allamountBorrow,allamountLoan,FSettleMethod,FSettleNumber,FWorkCenterName,FAcctreClassName,FSrcSeq,FStdSeq,FSeqNew,FIsdo,FMessage
  from rds_hrv_src_ds_middleTable where FBillNO in ((select FNumber from rds_hrv_src_ds_salary_input) UNION (select FNumber from rds_hrv_src_ds_socialsecurity_input))
  and fyear='%d.0' and fmonth='%d.0'",
                            var_hr_year,
                            var_hr_month
                          )
                          
                          
                          data = tsda::sql_select2(token = dms_token, sql = sql)
                          names(data) = c(
                            '日期',
                            '会计年度',
                            '会计期间',
                            '单据编号 ',
                            '凭证模板行号',
                            '凭证模板号',
                            '凭证模板名称',
                            '费用申报组织',
                            '费用承担组织',
                            '业务类型',
                            '摘要',
                            '账簿',
                            '往来单位名称',
                            '往来单位编码',
                            '供应商',
                            '供应商编码',
                            '银行账号',
                            '高新部门',
                            '科目编码',
                            '科目全名',
                            '核算维度',
                            '部门代码 ',
                            '部门名称',
                            'RD-项目（人工费用表格）',
                            '系统项目名称',
                            '责任中心',
                            '重分类',
                            '银行账号',
                            '借方金额',
                            '贷方金额',
                            '结算方式',
                            '结算号 ',
                            '责任中心名称',
                            '重分类名称',
                            'src行号',
                            'std新行号',
                            '过程表行号',
                            '状态',
                            '详细信息'
                          )
                          #显示数据
                          tsui::run_dataTable2(id = 'btn_hrv_voucher_data', data = data)
                        })
    
    
    
  })
  
  
}


#' Title 预览下载
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples downloadserver()
downloadvoucher <- function(input, output, session, dms_token) {
  # var_environment = tsui::var_ListChoose1('btn_hrv_voucher_environment')
  # var_hr_year = tsui::var_numeric('btn_hrv_voucher_year')
  # var_hr_month = tsui::var_numeric('btn_hrv_voucher_month')

  
  sql = 'select FDate,FYear,FMonth,FBillNO,FSeq,FNumber,FName,FTaxDeclarationOrg,FExpenseOrgID,FCategoryType,FNotes,FAccountBookID,
  FDealingUnitName,FDealingUnitNumber,FSupplierName,FSupplierNumber,FAccountName,FHightechDept,FSubjectNumber,FSubjectName,
  FLexitemProperty,FDeptNumber,FDeptName,FRdProject,FProjectNumber,FWorkCenterNumber,FAcctreClassNumber,FBankAccount,
  allamountBorrow,allamountLoan,FSettleMethod,FSettleNumber,FWorkCenterName,FAcctreClassName,FSrcSeq,FStdSeq,FSeqNew,FIsdo,FMessage
  from rds_hrv_src_ds_middleTable where FBillNO in ((select FNumber from rds_hrv_src_ds_salary_input) UNION (select FNumber from rds_hrv_src_ds_socialsecurity_input)) and fisdo=2'

  
  # var_environment = var_environment()
  # var_hr_year = var_hr_year()
  # var_hr_month = var_hr_month()
  
  
  # sql = sprintf(
  #   "select FDate,FYear,FMonth,FBillNO,FSeq,FNumber,FName,FTaxDeclarationOrg,FExpenseOrgID,FCategoryType,FNotes,FAccountBookID,
  # FDealingUnitName,FDealingUnitNumber,FSupplierName,FSupplierNumber,FAccountName,FHightechDept,FSubjectNumber,FSubjectName,
  # FLexitemProperty,FDeptNumber,FDeptName,FRdProject,FProjectNumber,FWorkCenterNumber,FAcctreClassNumber,FBankAccount,
  # allamountBorrow,allamountLoan,FSettleMethod,FSettleNumber,FWorkCenterName,FAcctreClassName,FSeqNew,FIsdo,FMessage
  # from rds_hrv_src_ds_middleTable where FBillNO in ((select FNumber from rds_hrv_src_ds_salary_input) UNION (select FNumber from rds_hrv_src_ds_socialsecurity_input))
  # and fyear='%d.0' and fmonth='%d.0' and fisdo=2 ",
  #   var_hr_year,
  #   var_hr_month
  # )
  
  data = tsda::sql_select2(token = dms_token, sql = sql)
  names(data) = c(
    '日期',
    '会计年度',
    '会计期间',
    '单据编号 ',
    '凭证模板行号',
    '凭证模板号',
    '凭证模板名称',
    '费用申报组织',
    '费用承担组织',
    '业务类型',
    '摘要',
    '账簿',
    '往来单位名称',
    '往来单位编码',
    '供应商',
    '供应商编码',
    '银行账号',
    '高新部门',
    '科目编码',
    '科目全名',
    '核算维度',
    '部门代码 ',
    '部门名称',
    'RD-项目（人工费用表格）',
    '系统项目名称',
    '责任中心',
    '重分类',
    '银行账号',
    '借方金额',
    '贷方金额',
    '结算方式',
    '结算号 ',
    '责任中心名称',
    '重分类名称',
    'src行号',
    'std新行号',
    '过程表行号',
    '状态',
    '详细信息'
  )
  # data2 = data[,c(4,1,2,3,8,9,6,7,5,)]
  data2 = data
  
  # sql = sprintf(
  #   "select FDate,FYear,FMonth,FBillNO,FSeq,FNumber,FName,FTaxDeclarationOrg,FExpenseOrgID,FCategoryType,FNotes,FAccountBookID,
  # FDealingUnitName,FDealingUnitNumber,FSupplierName,FSupplierNumber,FAccountName,FHightechDept,FSubjectNumber,FSubjectName,
  # FLexitemProperty,FDeptNumber,FDeptName,FRdProject,FProjectNumber,FWorkCenterNumber,FAcctreClassNumber,FBankAccount,
  # allamountBorrow,allamountLoan,FSettleMethod,FSettleNumber,FWorkCenterName,FAcctreClassName,FSeqNew,FIsdo,FMessage
  # from rds_hrv_src_ds_middleTable where FBillNO in ((select FNumber from rds_hrv_src_ds_salary_input) UNION (select FNumber from rds_hrv_src_ds_socialsecurity_input))
  # and fyear='%d.0' and fmonth='%d.0' and fisdo=0 ",
  #   var_hr_year,
  #   var_hr_month
  # )
  
  sql = 'select FDate,FYear,FMonth,FBillNO,FSeq,FNumber,FName,FTaxDeclarationOrg,FExpenseOrgID,FCategoryType,FNotes,FAccountBookID,
  FDealingUnitName,FDealingUnitNumber,FSupplierName,FSupplierNumber,FAccountName,FHightechDept,FSubjectNumber,FSubjectName,
  FLexitemProperty,FDeptNumber,FDeptName,FRdProject,FProjectNumber,FWorkCenterNumber,FAcctreClassNumber,FBankAccount,
  allamountBorrow,allamountLoan,FSettleMethod,FSettleNumber,FWorkCenterName,FAcctreClassName,FSrcSeq,FStdSeq,FSeqNew,FIsdo,FMessage
  from rds_hrv_src_ds_middleTable where FBillNO in ((select FNumber from rds_hrv_src_ds_salary_input) UNION (select FNumber from rds_hrv_src_ds_socialsecurity_input)) and fisdo=0'
  
  
  data = tsda::sql_select2(token = dms_token, sql = sql)
  names(data) = c(
    '日期',
    '会计年度',
    '会计期间',
    '单据编号 ',
    '凭证模板行号',
    '凭证模板号',
    '凭证模板名称',
    '费用申报组织',
    '费用承担组织',
    '业务类型',
    '摘要',
    '账簿',
    '往来单位名称',
    '往来单位编码',
    '供应商',
    '供应商编码',
    '银行账号',
    '高新部门',
    '科目编码',
    '科目全名',
    '核算维度',
    '部门代码 ',
    '部门名称',
    'RD-项目（人工费用表格）',
    '系统项目名称',
    '责任中心',
    '重分类',
    '银行账号',
    '借方金额',
    '贷方金额',
    '结算方式',
    '结算号 ',
    '责任中心名称',
    '重分类名称',
    'src行号',
    'std新行号',
    '过程表行号',
    '状态',
    '详细信息'
  )
  
  # data1 = data[,c(60:61,1:59,62:80)]
  data1 = data
  
  data = list(`成功数据` = data1, `异常数据` = data2)
  
  tsui::run_download_xlsx(id = 'btn_hrv_voucherview_download',
                          data = data ,
                          filename = '凭证生成下载.xlsx')
  
  
}


#' Title 下载日志
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples downloadserver()
downloadserver <- function(input, output, session, dms_token) {
  var_hr_year = tsui::var_numeric('btn_hrv_voucher_year')
  var_hr_month = tsui::var_numeric('btn_hrv_voucher_month')
  
  
  # var_hr_year = var_hr_year()
  # var_hr_month = var_hr_month()
  
  # sql1 = sprintf(
  #   "select * from rds_hrv_ods_ds_middleTable where fisdo=1 and fyear='%d.0' and fmonth='%d.0'",var_hr_year,var_hr_month)
  
  sql1 = 'select * from rds_hrv_ods_ds_middleTable where FBillNO in ((select FNumber from rds_hrv_src_ds_salary_input) UNION
                                                                                          (select FNumber from rds_hrv_src_ds_socialsecurity_input)) and fisdo=1'
  
  
  print(sql1)
  data1 = tsda::sql_select2(dms_token, sql1)
  names(data1) = c(
    '日期',
    '会计年度',
    '会计期间',
    '单据编号',
    '行号',
    '凭证模板号',
    '凭证模板名称',
    '费用申报组织',
    '费用承担组织',
    '业务类型',
    '摘要',
    '账簿',
    '往来单位名称',
    '往来单位编码',
    '供应商',
    '供应商编码',
    '账户名称',
    '高新部门',
    '科目编码',
    '科目全名',
    '核算维度',
    '部门代码 ',
    '部门名称',
    'RD-项目（人工费用表格）',
    '系统项目名称',
    '责任中心',
    '重分类',
    '银行账号',
    '借方金额',
    '贷方金额',
    '结算方式',
    '结算号 ',
    '详细信息',
    '状态',
    '责任中心名称',
    '重分类名称',
    'std新行号'
  )
  # tsui::run_download_xlsx(id = 'btn_Asone_download1',data = data1 ,filename = '成功数据.xlsx')
  
  
  # sql2 = sprintf(
  #   "select * from rds_hrv_ods_ds_middleTable where fisdo=2 and fyear='%d.0' and fmonth='%d.0'",
  #   var_hr_year,
  #   var_hr_month
  # )
  
  sql2 = 'select * from rds_hrv_ods_ds_middleTable where FBillNO in ((select FNumber from rds_hrv_src_ds_salary_input) UNION
                                                                                          (select FNumber from rds_hrv_src_ds_socialsecurity_input)) and fisdo=2'
  
  print(sql2)
  data2 = tsda::sql_select2(dms_token, sql2)
  names(data2) = c(
    '日期',
    '会计年度',
    '会计期间',
    '单据编号 ',
    '行号',
    '凭证模板号',
    '凭证模板名称',
    '费用申报组织',
    '费用承担组织',
    '业务类型',
    '摘要',
    '账簿',
    '往来单位名称',
    '往来单位编码',
    '供应商',
    '供应商编码',
    '账户名称',
    '高新部门',
    '科目编码',
    '科目全名',
    '核算维度',
    '部门代码 ',
    '部门名称',
    'RD-项目（人工费用表格）',
    '系统项目名称',
    '责任中心',
    '重分类',
    '银行账号',
    '借方金额',
    '贷方金额',
    '结算方式',
    '结算号 ',
    '详细信息',
    '状态',
    '责任中心名称',
    '重分类名称',
    'std新行号'
  )
  res = list(`成功数据` = data1, `异常数据` = data2)
  # print(res)
  
  
  tsui::run_download_xlsx(id = 'btn_hrv_voucher_download',
                          data = res ,
                          filename = '凭证同步至ERP日志.xlsx')
  
  
}


#' Title 后台处理总函数
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples HrvServer()
HrvServer <- function(input, output, session, dms_token) {
  #预览数据
  viewserver(input, output, session, dms_token)
  #上传数据
  uploadserver(input, output, session, dms_token)
  # #重新上传数据
  # reuploadserver(input, output, session, dms_token)
  #凭证预览
  viewvoucher(input, output, session, dms_token)
  #预览下载
  downloadvoucher(input, output, session, dms_token)
  #生成凭证
  voucherserver(input, output, session, dms_token)
  # # 重新生成凭证
  # revoucherserver(input, output, session, dms_token)
  # 下载日志
  downloadserver(input, output, session, dms_token)
}