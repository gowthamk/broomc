class Pair extends Object
{
  Object fst;
  Object snd;
  Pair(Object fst, Object snd) {
    this.fst = fst;
    this.snd = snd;
  }
  void swap() {
    Object temp = this.fst;
    this.fst = this.snd;
    this.snd = this.fst;
  }
}
