evaluation_metric = function(Net.true, Net.pred){
  
  Net.true = ((Net.true!=0)*1)*upper.tri(Net.true)
  Net.pred = ((Net.pred!=0)*1)*upper.tri(Net.true)
  
  
  
  FDP = 1 - sum(Net.pred*Net.true)/max(sum(Net.pred),1)
  power = sum(Net.pred*Net.true)/max(sum(Net.true),1)
  
  result = list(FDP=FDP, power=power)
  
}