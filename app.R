require(ggiraph)
require(ggplot2)
require(shiny)
require(DT)
require(plotly)



shinyApp(
    ui=fluidPage(
        
        h2("모집단선택"),
        radioButtons("choice",NULL,
                     choices=c("하나의 모집단"=1,"두개의 모집단"=2),inline=TRUE,selected=1),
        conditionalPanel(condition="input.choice==1",
                         h3("다중검정의 문제, 제1종오류와 유의수준"),
                         p("평균과 표준편차가 알려진 하나의 모집단에서 한 쌍의 표본을 추출하여 t.test를 시행할 경우
             0.05 이하의 p 값을 보일 수 있는 가능성은 5%이지만 여러 번 반복하는 경우 p값의 분포는
             random하게 나타납니다. 표본추출 횟수를 바꾸어 표본추출을 해보세요. 
             표본추출 버튼을 누를 때마다 표본을 다시 추출합니다. Primary outcome이 여러 개인 경우 다중검정의 문제가 생깁니다. ")
                         ),
        conditionalPanel(condition="input.choice==2",
                         h3("제2종오류와 검정력"),
                         p("평균이 다른 두개의 모집단에서 각각 표본을 추출하여 t.test를 시행할 경우
             0.05 이하의 p 값을 보일 수 있는 가능성은 평균의 차이, 표준편차, 표본의 수에 따라 달라집니다. 
             표본추출 버튼을 누를 때마다 표본을 다시 추출합니다.")
        ),
        
        hr(),
        fluidRow(
            column(2,numericInput("n", label = "표본추출횟수:",value=100,step=1)),   
            column(2,h4(""),
                   actionButton("sample","표 본 추 출")),
            column(8,
                   fluidRow(
                       conditionalPanel(condition = "input.choice=='2'",
                                        column(3,h4("Group A"))),   
                       column(3,numericInput("mean", label = "평균:",value=0,step=0.1)),
                       column(3,numericInput("sd", label = "표준편차:",value=1,step=1)),
                       column(3,numericInput("size", label = "표본크기:",value=30,step=1))
                       
                   ),
                   fluidRow(
                       conditionalPanel(condition = "input.choice=='2'",
                                        column(3,h4("Group B")),   
                                        column(3,numericInput("mean1", label = "평균:",value=0.5,step=1))
                       )) 
            )
        ),
        numericInput("pval",label="유의수준",value=0.05,step=0.01,min=0,max=1),
        hr(),
        tabsetPanel(
            tabPanel("p값 그래프1",
                     
                     h5("표본추출을 반복한 결과 그래프입니다. 그래프 위의 점에 마우스 커서를 갖다 대면
                    표본추출번호와 p값을 볼 수 있습니다.
                    p값그래프2는 p값 순서대로 재배열한 그래프입니다.
                    임의의 한 점을 클릭하여 선택하면 해당하는 표본 자료를 볼  수 있습니다."),
                     ggiraphOutput("plot1",height="600px")),
            tabPanel("p값 그래프2",
                     conditionalPanel(condition="input.choice=='2'",
                                      checkboxInput("power","검정력표시",value=TRUE),
                                      checkboxInput("beta","2종에러표시",value=FALSE)
                     ),
                     ggiraphOutput("plot2",height="600px")),
            tabPanel("표본자료보기",
                     fluidRow(
                         column(4,
                                p("n번째 추출된 자료 분포 보기:"),
                                
                                sliderInput("no", "n번째 자료선택",min=1,max=100,value=1,step=1,animate=TRUE),
                                
                                dataTableOutput("table1")),
                         column(8,
                                plotOutput("plot3"),
                                verbatimTextOutput("stats")))),
            
            
            tabPanel("Primary Outcome의 갯수",
                     h3("Primary outcome의 갯수와 우연히 의미있게 나올 확률"),
                     
                     p("같은 모 집단에서 무작위로 추출한 두개의 집단의 차이를 볼 경우를 예로 들겠습니다. 하나의 primary outcome을 보는 경우 p 값이 0.05이하로 나올 확률은 5%정도 입니다. 하지만 primary outcome이 두개인 경우 둘 중에 하나라도 0.05이하로 나올 확률은 1-(0.95*0.95)*100이므로 9.75% 입니다. 마치 20면 주사위 두개를 던져 하나라도 1이 나올 가능성과 같습니다. "),
                     imageOutput("image",height="200px"),
                     plotlyOutput("plot5",width="60%",height="600px")  
                     
            ),
            tabPanel("표본크기와 검정력",
                     plotlyOutput("plot4",width="60%",height="600px")        
            )
            
        )
    ),
    server=function(input,output,session){
        
        tooltip_css <- "background-color:white;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;"
        hover_css="r:4px;cursor:pointer;stroke-width:6px;"
        
        
        observe({
            updateSliderInput(session,"no",max=input$n)
        })
        
        selected_try<-reactive({
            if(is.null(input$plot1_selected)){
                character(0)
            } else input$plot1_selected
        })
        
        selected_try2<-reactive({
            if(is.null(input$plot2_selected)){
                character(0)
            } else input$plot2_selected
        })
        
        observe({
            value<-selected_try()
            updateSliderInput(session,"no",value=value)
        })
        observe({
            value<-selected_try2()
            updateSliderInput(session,"no",value=value)
        })
        
        df=reactive({
            input$sample
            
            isolate({
                res=c()
                A=list()
                B=list()
                
                for(i in 1:input$n){
                    a=rnorm(n=input$size,mean=input$mean,sd=input$sd)
                    if(input$choice=="1"){
                        b=rnorm(n=input$size,mean=input$mean,sd=input$sd)
                    } else {
                        b=rnorm(n=input$size,mean=input$mean1,sd=input$sd)
                    }
                    A[[i]]=a
                    B[[i]]=b
                    summary(a)
                    summary(b)
                    result1=var.test(a,b)
                    result1
                    if(result1$p.value<0.05) {
                        result=t.test(a,b)
                    } else {
                        result=t.test(a,b,var.equal=TRUE)
                    }
                    result$p.value
                    res=c(res,result$p.value)
                }
                x=1:length(res)
                y=res
                
                C=as.numeric(reorder(x,y))
                df=data.frame(x=x,y=y,C=C)
                df$tooltip=paste0("try=",df$x,"<br>p=",round(df$y,4))
                df$p_value=ifelse(df$y<=input$pval,paste0("p < ",input$pval),"p = NS")
                result<-list(A=A,B=B,df=df)
                result
            })
            
        })
        
        output$text=renderPrint({
            data<-df()
            str(data)
            
        })
        output$plot1<-renderggiraph({
            data<-df()$df
            label=paste0("No of p < ",input$pval," = ",sum(data$y<=input$pval)," / ",nrow(data)," attempts")
            p<-ggplot(data=data,aes(x=x,y=y,data_id=x,tooltip=tooltip,colour=p_value))+
                geom_hline(yintercept=input$pval,colour="red",linetype=2,size=0.5)+
                geom_point_interactive()+
                labs(title=label,x="표본추출번호",y="p-value")
            ggiraph(code=print(p),
                    selection_type="single",
                    tooltip_extra_css=tooltip_css,tooltip_opacity=.75,zoom_max=10,hover_css=hover_css)
            
        })
        output$plot2=renderggiraph({
            data<-df()$df
            count=sum(data$y<=input$pval)
            label=paste0("No of p < ",input$pval," = ",count," / ",nrow(data)," attempts")
            p<-ggplot(data=data,aes(x=C,y=y,data_id=x,tooltip=tooltip,colour=p_value))+
                geom_point_interactive()+
                labs(title=label,y="p-value",x="")
            if(input$choice=='1'){
                p<-p+geom_hline(yintercept=input$pval,colour="red",linetype=2,size=0.5)
            } else{
                p<-p+geom_vline(xintercept=count,colour="red",linetype=2,size=0.5)
                if(input$power){
                    p<-p+geom_segment(x=0,y=input$pval,xend=count,yend=input$pval,
                                      colour="red",size=0.5,
                                      arrow = grid::arrow(angle=20,length = unit(0.03, "npc"),
                                                          end="both",type="closed"))
                    p<-p+annotate("text",x=count/2,y=input$pval,
                                  label=paste0("검정력: ",round(count/nrow(data),2)),vjust=-1.5)
                }
                if(input$beta){
                    p<-p+geom_segment(x=count,y=input$pval,xend=input$n,yend=input$pval,
                                      colour="red",size=0.5,
                                      arrow = grid::arrow(angle=20,length = unit(0.03, "npc"),
                                                          end="both",type="closed"))
                    
                    p<-p+annotate("text", x=(count+input$n)/2,y=input$pval,label="제2종오류",vjust=-1.5)
                }
            }
            
            ggiraph(code=print(p),
                    selection_type="single",
                    tooltip_extra_css=tooltip_css,tooltip_opacity=.75,zoom_max=10,hover_css=hover_css)
            
        })
        output$table1<-DT::renderDataTable({
            data=df()     
            A=round(data$A[[input$no]],4)
            B=round(data$B[[input$no]],4)
            result<-data.frame(A,B)
            DT::datatable(result)
        })
        output$stats<-renderPrint({
            data=df()     
            A=data$A[[input$no]]
            B=data$B[[input$no]]
            cat("A 통계량\n")
            print(summary(A))
            cat("\nB 통계량\n")
            print(summary(B))
            result1=var.test(A,B)
            result1
            if(result1$p.value<0.05) {
                result=t.test(A,B)
            } else {
                result=t.test(A,B,var.equal=TRUE)
            }
            #cat("\nt.test 결과\n")
            print(result)
        })
        output$plot3<-renderPlot({
            data=df()     
            A=data$A[[input$no]]
            B=data$B[[input$no]]
            df=data.frame(group=c(rep("A",input$size),rep("B",input$size)),value=c(A,B))
            
            ggplot(data=df,aes(x=value,fill=group))+geom_density(alpha=0.3)
            
            
        })
        
        output$plot4<-renderPlotly({
            muA=input$mean
            muB=input$mean1
            kappa=1
            sd=input$sd
            alpha=0.05
            power=seq(0.5,0.95,0.01)
            beta=1-power
            (nB=(1+1/kappa)*(sd*(qnorm(1-alpha/2)+qnorm(1-beta))/(muA-muB))^2)
            df=data.frame(x=power,y=nB)
            subtitle=paste0("mean difference:",abs(muA-muB),",sd:",sd)
            power1=0.8
            nB1=(1+1/kappa)*(sd*(qnorm(1-alpha/2)+qnorm(0.8))/(muA-muB))^2
            p=ggplot(data=df,aes(x,y))+geom_line()+
                labs(x="power",y="Sample size",title="Power and sample size",subtitle=subtitle)
            ggplotly(p)
        })
        output$plot5<-renderPlotly({
            n=c(1:100)
            AtLeastOne=function(n){
                1-0.95^n
            }
            AtLeast=function(x){
                result=c()
                for(i in 1:length(x)){
                    result=c(result,AtLeastOne(x[i]))
                }
                result
            }
            
            #AtLeast(n)
            df=data.frame(n=n,ratio=AtLeast(n))
            #df$tooltip=paste0("n=",df$x,"<br>",round(df$y*100,3)," %")
            p<-ggplot(data=df,aes(x=n,y=ratio,group=1))+geom_line()+xlab("Primary outcome의 갯수")+ylab("하나 이상의 유의한 결과를 보일 가능성")
            ggplotly(p)    
        })
        
        output$image=renderImage({
            list(
                src="Dice.jpg",
                contentType="image/jpeg",
                alt="Dices"
            )
        },deleteFile = FALSE)
        
    },
    options = list(
        width = "100%", height = 1300
    )
)
