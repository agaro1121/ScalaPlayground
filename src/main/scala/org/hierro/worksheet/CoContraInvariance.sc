class C
class CPP extends C
class Java extends CPP
class Scala extends Java

class CoVar[+A]
class InVar[A]
class ContraVar[-A]

def covar(x: CoVar[Java]): CoVar[Java] = identity(x)
def invar(x: InVar[Java]): InVar[Java] = identity(x)
def contravar(x: ContraVar[Java]): ContraVar[Java] = identity(x)

//only type and subType allowed
covar(new CoVar[Scala])
covar(new CoVar[Java])
//covar(new CoVar[CPP]) //not a subtype of Java Class

//only type allowed
//invar(new InVar[Scala]) //only Java class allowed
invar(new InVar[Java])
//invar(new InVar[CPP]) //only Java class allowed

//only type or superType allowed
contravar(new ContraVar[Java]) //Same clsss allowed
//contravar(new ContraVar[Scala]) //not allowed cuz it's a subtype
contravar(new ContraVar[CPP]) //allowed because contra means supertypes


val x = 5
println("id="+identity(x))