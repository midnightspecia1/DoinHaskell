
--fighting robbots
robot (name,power,health) = \message -> message (name,power,health)

--all objects can be viwed like a collection of attributes that you send message to 
--making accessors 
name (n,_,_) = n
power (_,p,_) = p
health (_,_,h) = h

getAttribute attribute robot = robot attribute
getName robot = robot name
getPower robot = robot power
getHealth robot = robot health 



--making setters
setName aRobot newName = robot (newName, (getPower aRobot), (getHealth aRobot))
setPower aRobot newPower = robot ((getName aRobot), newPower, (getHealth aRobot))
setHealth aRobot newHealth = robot ((getName aRobot), (getPower aRobot), newHealth)

--print function toString like
printR aRobot = aRobot (\(a,b,c) -> a ++ " Robot power is: " ++ show b ++ " Robot health is: " ++ show c)

--damage function 
damage aRobot damageCount = aRobot (\(a,b,c)-> robot (a,b,c - damageCount))

--fight function
fight attackerR defenderR = damage defenderR attack
                            where attack = if getHealth attackerR > 0
                                           then getPower attackerR
                                           else 0

-- --three round fight
-- threeRoundfight rA rB = let rAFinal = foldl damage rB [dmgA, dmgA, dmgA]
--                             rBFinal = foldl fight rA [dmgB, dmgB, dmgB]

--                         in 
--                             if getHealth rAFinal > getHealth rBFinal
--                             then rAFinal
--                             else rBFinal
    

------------------------------------------
fastRobot = robot ("fast", 12, 40)
slowRobot = robot ("slow", 15, 70)
anotherOne = robot("another", 30, 28)

slowRobotRound3 = fight fastRobotRound2 slowRobotRound2
slowRobotRound2 = fight fastRobotRound1 slowRobotRound1
slowRobotRound1 = fight fastRobot slowRobot
fastRobotRound3 = fight slowRobotRound3 fastRobotRound2 
fastRobotRound2 = fight slowRobotRound1 fastRobotRound1 
fastRobotRound1 = fight slowRobot fastRobot