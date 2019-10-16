function GenericBox() {
  const box = document.createElement('div');
  box.classList.add('gameUIContainer');
  return box;
}

GenericBox.prototype.constructor = GenericBox;

export default GenericBox;
