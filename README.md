# Covid-19_infection-isolation_model
Covid-19_infection&amp;isolation_model


## infection_loop.f90
用來處理測資數據  
並根據呼叫之隔離演算法  
計算隔離完後的感染人數

## link_algorithm_beta.f90
隔離演算法  
考慮傳染鏈長短  
感染力強度  
傳染成功率  
並計算關鍵傳染者

## isolate_by_random.py
隨機挑選隔離者  
用來參考隔離演算法好壞的對照組


## test_data.f90
測資內含傳染強度、抵抗力、傳染鏈  
(若A與B存在傳染鏈，A之傳染力大於B之抵抗力，則成功傳染)

