class Simple extends Object
{
  Object x;
  Object y;
  Simple(Object x,Object y) {
    this.x = x;
    this.y = y;
  }
  void swap() {
    Object tmp = this.x;
    this.x = this.y;
    this.y = tmp;
  }
}
