

--cup of coffee
--cup ml = \_ -> ml

cup ml = \message -> message ml

--Getter for the ml
getMl aCup = aCup (\ml -> ml)

--defining a drink message
drink aCup mlToDrink = if mlDiff < 0
                       then cup 0
                       else  cup mlDiff
                       where mlRightNow = getMl aCup
                             mlDiff = mlRightNow - mlToDrink

--defining is cup empty
isEmpty aCup = getMl aCup == 0

