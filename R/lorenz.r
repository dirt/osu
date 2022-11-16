myplot <- function (vecNum)
{
df =data.frame("var1"=1:length(vecNum), "var2"=vecNum);
p   = ggplot(df, aes_string(x="var1", y="var2" ))+geom_point();
print(p)
return(p)
}