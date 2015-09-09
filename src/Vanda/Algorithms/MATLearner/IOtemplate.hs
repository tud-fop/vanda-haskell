class Teacher a where 
  teacherFunction :: a -> IO a
  
instance Teacher Int where
  teacherFunction i = do
    -- with IO in called functions
    r <- importantIntTeacherFunctionwithIO i
    return r

instance Teacher Bool where
  teacherFunction b = do
    -- no IO necessary
    return $ importantBoolTeacherFunctionwithoutIO b


main :: IO ()
main = do
  a <- teacherFunction nonIOTeacher
  b <- teacherFunction ioTeacher
  putStrLn $ show a
  putStrLn $ show b

ioTeacher :: Int
ioTeacher = 0

nonIOTeacher :: Bool
nonIOTeacher = True

importantIntTeacherFunctionwithIO :: Int -> IO Int
importantIntTeacherFunctionwithIO i = do
  putStrLn "Calculating important IO Stuff..."
  return $ i + 1
  
importantBoolTeacherFunctionwithoutIO :: Bool -> Bool
importantBoolTeacherFunctionwithoutIO b = not b