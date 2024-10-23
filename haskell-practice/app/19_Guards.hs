-- Guards in Haskell are a way to add conditional logic to functions based on various expressions that evaluate to True or False. 
-- They allow us to express complex conditions in a more readable and structured way than nested if-else statements. 
-- A guard is essentially a boolean expression that allows a function to execute 
-- different code based on whether certain conditions are met. 
-- Guards are written using the | symbol, followed by the condition, and then the result to return if that condition is True. 
--     They make it straightforward to define cases based on ranges or specific criteria.

-- Syntax

-- functionName args
--     | condition1 = result1
--     | condition2 = result2
--     | ...
--     | otherwise = defaultResult

-- Function name and arguments: You define the function and provide arguments as usual.
-- Guard conditions (| condition1, etc.): These are boolean expressions that evaluate to True or False.
-- Results: Each guard is followed by the result to return if that condition is True.
-- Otherwise: The otherwise keyword is a catch-all guard that is equivalent to True and serves as a fallback.


-- Example Explanation: gradeFromGPA

-- gradeFromGPA :: Int -> String
-- gradeFromGPA gpa
--     | gpa >= 18 = "A"
--     | gpa >= 15 = "B"
--     | gpa >= 12 = "C"
--     | otherwise = "below C"

-- Input: An integer representing the GPA.
-- Guards:
-- gpa >= 18 results in "A".
-- gpa >= 15 results in "B".
-- gpa >= 12 results in "C".
-- otherwise catches all cases below 12, returning "below C".
-- Execution: The function checks the first guard, then moves down the list. 
-- Once it finds a True guard, it returns the associated result and ignores the remaining guards.


-- Check Age Group
ageGroup :: Int -> String
ageGroup age
    | age < 13  = "Child"
    | age < 20  = "Teenager"
    | age < 65  = "Adult"
    | otherwise = "Senior"
-- This function classifies a person’s age group based on the age input.

-- Speed Category
speedCategory :: Int -> String
speedCategory speed
    | speed <= 20 = "Very slow"
    | speed <= 40 = "Slow"
    | speed <= 60 = "Moderate"
    | speed <= 80 = "Fast"
    | otherwise   = "Very fast"
-- Categorizes speed values based on a range.

-- Water Temperature State
waterState :: Double -> String
waterState temp
    | temp <= 0  = "Solid (Ice)"
    | temp < 100 = "Liquid"
    | otherwise  = "Gas (Steam)"
-- Determines the state of water at different temperatures.

-- BMI Classification
bmiCategory :: Double -> String
bmiCategory bmi
    | bmi < 18.5 = "Underweight"
    | bmi < 24.9 = "Normal weight"
    | bmi < 29.9 = "Overweight"
    | otherwise  = "Obesity"
-- This function categorizes BMI (Body Mass Index) into common classifications.

-- Tax Rate Calculation
taxBracket :: Double -> String
taxBracket income
    | income <= 9875   = "10%"
    | income <= 40125  = "12%"
    | income <= 85525  = "22%"
    | income <= 163300 = "24%"
    | income <= 207350 = "32%"
    | income <= 518400 = "35%"
    | otherwise        = "37%"
-- Determines the tax bracket based on income levels.

-- Traffic Light Suggestion
trafficLight :: String -> String
trafficLight color
    | color == "Red"    = "Stop"
    | color == "Yellow" = "Caution"
    | color == "Green"  = "Go"
    | otherwise         = "Invalid color"
-- Suggests an action based on the color of a traffic light.

-- Risk Assessment Based on Age and Medical Condition
riskAssessment :: Int -> Bool -> String
riskAssessment age hasCondition
    | age >= 65 && hasCondition = "High Risk"
    | age >= 65                 = "Moderate Risk"
    | age < 65 && hasCondition  = "Increased Risk"
    | otherwise                 = "Low Risk"
-- Assesses health risk level based on age and whether the person has a medical condition.

-- Employee Bonus Eligibility
bonusEligibility :: Int -> Int -> String
bonusEligibility yearsWorked performanceRating
    | yearsWorked >= 10 && performanceRating >= 8 = "High Bonus"
    | yearsWorked >= 5  && performanceRating >= 6 = "Medium Bonus"
    | performanceRating >= 4                      = "Low Bonus"
    | otherwise                                   = "No Bonus"
-- Determines bonus eligibility based on years of service and performance rating.

-- Fuel Efficiency Rating
fuelEfficiency :: Double -> Double -> String
fuelEfficiency distance fuel
    | rate >= 20 = "Excellent"
    | rate >= 15 = "Good"
    | rate >= 10 = "Average"
    | rate >= 5  = "Poor"
    | otherwise  = "Very Poor"
    where rate = distance / fuel
-- Rates fuel efficiency by calculating distance per unit of fuel, using a guard with a where clause to define the rate variable.
