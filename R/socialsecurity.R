#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' socialsecurityServer()
socialsecurityServer <- function(input,output,session,dms_token) {
  var_text_hrv_voucher_year=tsui::var_numeric("text_hrv_voucher_year")
  var_text_hrv_voucher_month=tsui::var_numeric("text_hrv_voucher_month")
  var_text_hrv_voucher_environment=tsui::var_ListChoose1("text_hrv_voucher_environment")
  
  shiny::observeEvent(input$btn_socialsecurity,{
    token=dms_token
    FYear=var_text_hrv_voucher_year()
    FMonth=var_text_hrv_voucher_month()
    FExpenseOrgID=var_text_hrv_voucher_environment()
    
    
    
    #按承担组织清除--工资
    
    jhhrvvoucherpkg::stdsocialsecurity(token = token,FYear =FYear ,FMonth =FMonth ,FExpenseOrgID =FExpenseOrgID )
    
    
    jhhrvvoucherpkg::srcsocialsecurity(token = token,FYear =FYear ,FMonth =FMonth ,FExpenseOrgID =FExpenseOrgID )
    
    jhhrvvoucherpkg::odssocialsecurity(token = token,FYear =FYear ,FMonth =FMonth ,FExpenseOrgID =FExpenseOrgID )
    
    #按--清除中间表
    str <-".0"
    FYear2=sprintf("%s%s",FYear ,str)
    FMonth2=sprintf("%s%s",FMonth ,str)
    
    jhhrvvoucherpkg::srcmiddleTable(token = token,FYear =FYear2 ,FMonth =FMonth2 ,FExpenseOrgID =FExpenseOrgID )
    jhhrvvoucherpkg::odsmiddleTable(token = token,FYear =FYear2 ,FMonth =FMonth2 ,FExpenseOrgID =FExpenseOrgID )
    
    
    tsui::pop_notice("社保相关数据已清除成功")
    
    
    
    
    
    
  })
}
