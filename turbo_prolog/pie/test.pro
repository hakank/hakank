under_30( Person )  :- person( Person , Age , _ ) , Age < 30.
over_30( Person )   :- person( Person , Age , _) , Age >= 30.
person( hakan  ,   31 , man ).
person( helene ,   24 , kvinna ).
person( jan    ,   29 , man ).
person( anna   ,   20 , kvinna ).
person( olle   ,   30 , man ).
person( anders ,   35 , man ).
person( kurt   ,   31 , man ).
person( birgitta , 35 , kvinna ).
sex (Person , Sex ) :- person( Person , _  , Sex ).
kvinna( Person )    :- sex( Person  , kvinna ).
man( Person )       :- sex( Person , man ).
brud( Person )      :- under_30( Person ) , kvinna( Person ).
kille( Person )     :- under_30( Person ) , man( Person ).
karing( Person )    :- over_30( Person ) , kvinna( Person ).
gubbe( Person )     :- over_30( Person ) , man( Person ).









