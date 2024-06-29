"""
? Exercício 3.5
 * Data de entrega: domingo, 19 Jun 2022, 23:59
 * Arquivos requeridos: user.py ( Baixar)
 * Tipo de trabalho: Trabalho individual

! Considere uma heap e seu administrador abaixo,
! a classe HeapManager, em Python. Ela utiliza uma
! estratégia first-fit para encontrar o primeiro bloco
! de memória grande o suficiente para alocar uma requisição.
! Porém ela possui apenas a opção para alocamento de espaço
! na memória. Você deverá implementar a função deallocate(),
! para assim finalizar as funcionalidades de gerenciamento
! desta heap. Você pode assumir que o endereço recebido
! sempre é o inicio de um bloco alocado. Note que não é
! necessário implementar nenhuma estratégia de desframgentação.

?input:
? h = HeapManager(14)
? a = h.allocate(4)
? b = h.allocate(2)
? c = h.allocate(3)
? h.deallocate(a)
? h.deallocate(c)
? d = h.allocate(2)

output:
  d = 1

TODO h.memory = [3, 8, 0, 2, 8, 3, -1, 0, 4, 12, 0, 0, 3, -1, 0]
"""

NULL = -1 # The null link

class HeapManager :
  """ Implements a very simple heap manager ."""

  def __init__(self,memorySize) :
    """ Constructor . Parameter initialMemory is the array of
        data that we will
        use to represent the memory ."""
    self.memory = [0] * memorySize
    self.memory[0] = self.memory.__len__()
    self.memory[1] = NULL
    self.freeStart = 0

  def allocate(self, requestSize) :
    """Allocates a block of data , and return its address . 
    The parameter requestSize is the amount of space that must be allocaed."""
    DEBUG = False

    size = requestSize + 1
    # Do first-fit search: linear search of the free list for the first block
    # of sufficient size .
    p = self.freeStart
    if DEBUG: 
      print('freeStart:',p)

    lag = NULL
    if DEBUG: 
      print('lag:',lag)

    while p != NULL and self.memory[p] < size :
      lag = p
      if DEBUG: 
        print('p = self.memory[',p+1,end='')

      p = self.memory[p + 1]
      if DEBUG: 
        print('] = ',p)

      if DEBUG: 
        print('lag:',lag)

    if p == NULL :
      raise MemoryError()
    if DEBUG: 
      print('nextFree = self.memory[',p+1,end='')

    nextFree = self.memory[p + 1]
    if DEBUG: 
      print(']=',nextFree)


    # Now p is the index of a block of sufficient size ,
    # lag is the index of p’s predecessor in the
    # free list , or NULL , and nextFree is the index of
    # p’s successor in the free list , or NULL .
    # If the block has more space than we need , carve
    # out what we need from the front and return the
    # unused end part to the free list .
    if DEBUG: 
      print(f'unused = self.memory[{p}-{size}]',end='')

    unused = self.memory[p] - size
    if DEBUG: 
      print('=',unused)

    if unused > 1:
      if DEBUG: 
        print('unused > 1')

      if DEBUG: 
        print(f'nextFree = {p} + {size}',end='')

      nextFree = p + size
      if DEBUG: 
        print('=',nextFree)

      self.memory[nextFree] = unused

      if DEBUG: 
        print(f'self.memory[{nextFree} + 1] = self.memory[{p} + 1]',end='')

      self.memory[nextFree + 1] = self.memory[p + 1]
      if DEBUG: 
        print('=',self.memory[nextFree+1])

      if DEBUG: 
        print(f'self.memory[{p}] = {size}')

      self.memory[p] = size

    if lag == NULL:
      if DEBUG: 
        print('lag==NULL')

      if DEBUG: 
        print(f'self.freeStart = {nextFree}')

      self.freeStart = nextFree
      
    else :
      if DEBUG: 
        print('lag!=NULL')

      if DEBUG: 
        print(f'self.memory[{lag} + 1] = {nextFree}')

      self.memory[lag + 1] = nextFree

    return p + 1
    
  # complete a função abaixo
  def deallocate(self, address):
    return
    print('wait',address)
    jmpsize = self.memory[address - 1]
    print(jmpsize)


  def show(self):
    print(self.memory)

if __name__ == '__main__':
  h = HeapManager(14)
  h.show()
  # [14, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  a = h.allocate(4)
  h.show()
  # [5, -1, 0, 0, 0, 9, -1, 0, 0, 0, 0, 0, 0, 0]
  b = h.allocate(2)
  h.show()
  # [5, -1, 0, 0, 0, 3, -1, 0, 6, -1, 0, 0, 0, 0]
  c = h.allocate(3)
  h.show()
  # [5, -1, 0, 0, 0, 3, -1, 0, 4, -1, 0, 0, 2, -1]
  h.deallocate(a)

  h.deallocate(c)

  d = h.allocate(2)

  print('d =',d)
  # d = 1
  h.show()
  # [3, 8, 0, 2, 8, 3, -1, 0, 4, 12, 0, 0, 3, -1, 0]

"""
[14, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
{}
[5, -1, 0, 0, 0, 9, -1, 0, 0, 0, 0, 0, 0, 0]
 a               x
[5, -1, 0, 0, 0, 3, -1, 0, 6, -1, 0, 0, 0, 0]
 a               b         x
[5, -1, 0, 0, 0, 3, -1, 0, 4, -1, 0, 0, 2, -1]
 a               b         c            x

deallocate(a)
[5, -1, 0, 0, 0, 3, -1, 0, 4, -1, 0, 0, 2, -1]
 a               b         c            x

[5, -1, 0, 0, 0, 3, -1, 0, 4, -1, 0, 0, 2, -1]
[3,  8, 0, 2, 8, 3, -1, 0, 4, 12, 0, 0, 3, -1, 0]


[3, 8, 0, 2, 8, 3, -1, 0, 4, 12, 0, 0, 3, -1, 0]
"""



