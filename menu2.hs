main = do
 dev True
dev con = do
    if con
        then do
        
        putStrLn "*************Menu**************"
     

     
        putStrLn ("1.-Fibonacci")
        putStrLn ("2.-Presentar Numeros del 1-10.")
        putStrLn ("3.-Factorial.")
        putStrLn ("4.-Desaparece Numeros.")
        putStrLn ("5.-Palindromos.")
        putStrLn ("6.-Menu de Calculadora.")
        putStrLn ("7.-Salir")
        op <- getLine

        case op of
            "1" -> do
                   putStrLn "Posicion:" 
                   n <- getLine
                   let nInt = read n::Int
                   fibo nInt
            "2" -> do
                 numeros 1

            "3" -> do
                    putStrLn("Ingresa un numero:")
                    numero <-getLine
                    print ("factorial es:"++ show (factorial(read numero)))
                    dev True
            "4" -> do
                    arreglo [0,1,2,3,4,5,6,7,8,9,10]
                    dev True
            "5" -> do
                putStrLn ("Ingresa la frase:")
                palabra <- getLine
                palin palabra
            "6" -> do
                    putStrLn "*********Calculadora**********"
                    putStrLn "1.- Suma"
                    putStrLn "2.- Resta"
                    putStrLn "3.- Multiplicacion"
                    putStrLn "4.- Division"
                    putStrLn("Elige la operacion a realizar") 
                    n <- getLine
                    casos n
            "7" -> do
                dev False
            _ -> print("Opcion invalida")
          
    else
         putStrLn "Salistes del menu"


fibo nInt= do
    let arreglo = ["8", "13" , "0", "3" ,"32","9","74", "1","37","2"]

    if(nInt<= 9)
        then do
            print (arreglo !! nInt)
            dev True
    else do
        dev True


numeros n = do
    if n<=10
        then do
            print n 
            numeros (n+1)
      else do
            dev True

factorial n= if n==0 then 1
            else n*factorial(n-1)

arreglo arr= 
    if null arr
        then 
            print("-")
    else do
        print (arr)
        arreglo(init arr)

palin palabra= do
    let res = palabra== reverse palabra

    if res==True
        then do
            putStrLn ("Es un palindromo")
            dev True
    else do
        putStrLn ("No es un palindromo")
        dev True


casos  n= do
    case n of 
        "1" ->suma
        "2" ->resta
        "3" ->multiplicacion
        "4" ->division
        _ -> print ("Opcion invalida")

suma = do
    putStrLn("numero 1: ")
    a <- readLn    
    putStrLn("numero 2: ")
    b <- readLn
    print ("El resultado es "++ show(a+b))
    dev True

resta = do
    putStrLn("numero 1: ")
    a <- readLn    
    putStrLn("numero 2: ")
    b <- readLn
    print ("El resultado es "++ show(a-b))
    dev True

multiplicacion = do
    putStrLn("numero 1: ")
    a <- readLn    
    putStrLn("numero 2: ")
    b <- readLn
    print ("El resultado es "++ show(a*b))
    dev True

division = do
    putStrLn("numero 1: ")
    a <- readLn    
    putStrLn("numero 2: ")
    b <- readLn
    print ("El resultado es "++ show(a/b))
    dev True

----------------------------------------------------------------
