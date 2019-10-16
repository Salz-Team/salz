import GenericBox from './genericBox';

/**
 * CellInfoBox class
 *
 * @param   {Number}    x     x coordinate of box upper left corner
 * @param   {Number}    y     y coordinate of box upper left corner
 * @param   {Object}    data  Data to be shown in the box
 */
function CellInfoBox(x, y, data) {
  const box = GenericBox.call();
  box.classList.add('cellInfoContainer');

  const xcoord = x + 20;
  const ycoord = y - 30;
  box.style.top = ycoord + 'px';
  box.style.left = xcoord + 'px';

  const boxInfo = document.createElement('ul');
  for (const key in data) {
    const listItem = document.createElement('li');
    listItem.innerHTML = key + ': ' + data[key];
    boxInfo.appendChild(listItem);
  }

  box.appendChild(boxInfo);

  return box;
}

CellInfoBox.prototype = Object.create(GenericBox.prototype);
CellInfoBox.prototype.constructor = CellInfoBox;

export default CellInfoBox;
