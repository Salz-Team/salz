import Grid, { Cell } from './conway';

describe('cell at coordinate (1, 2)', () => {
  const cell = new Cell(1, 2);
  test('has id -1 as the default owner', () => {
    expect(cell.owner === -1).toBeTruthy();
  });

  test('can set owner to a different owner', () => {
    cell.owner = 2;
    expect(cell.owner === 2).toBeTruthy();
  });
});

describe('game grid', () => {
  const grid = new Grid(25);
  const snapshot = {
    turnid: 1,
    cells: [
      { x: 3, y: 3, playerid: 1 },
      { x: 2, y: 3, playerid: 1 },
      { x: 4, y: 3, playerid: 1 },
    ],
  };

  test('can construct a grid of size 25', () => {
    expect(grid.size === 25).toBeTruthy();
    expect(grid.size !== 24).toBeTruthy();
  });

  test('starts with an empty collection of cells', () => {
    expect(grid.cells.length === 0).toBeTruthy();
  });

  test('can mount a snapshot with 3 cells', () => {
    grid.mountSnapshot(snapshot);
    expect(grid.cells.length === 3).toBeTruthy();
  });
});

describe('game rules', () => {
  const grid = new Grid(50);
  const snapshot = {
    turnid: 1,
    cells: [
      { x: 3, y: 3, playerid: 1 },
      { x: 2, y: 3, playerid: 1 },
      { x: 4, y: 3, playerid: 1 },
    ],
  };
  grid.mountSnapshot(snapshot);

  const moves = [
    {
      turnid: 1,
      moves: [
        { x: 10, y: 10, playerid: 1 },
        { x: 15, y: 15, playerid: 1 },
        { x: 25, y: 25, playerid: 1 },
      ],
    },
    {
      turnid: 2,
      moves: [
        { x: 5, y: 2, playerid: 1 },
        { x: 1, y: 4, playerid: 1 },
        { x: 25, y: 25, playerid: 1 },
      ],
    },
  ];

  test('properly iterates to the first step', () => {
    grid.next(moves[0].moves);
    expect(grid.cells.length === 3).toBeTruthy();
  });

  test('properly iterates to the second step', () => {
    grid.next(moves[1].moves);
    expect(grid.cells.length === 3).toBeTruthy();
    expect(grid.cells.filter(cell => cell.equals({ x: 4, y: 2 })).length !== 0).toBeTruthy();
    expect(grid.cells.filter(cell => cell.equals({ x: 2, y: 4 })).length !== 0).toBeTruthy();
  });

  test('properly iterates to the third step (without new moves)', () => {
    grid.next([]);
    expect(grid.cells.length === 1).toBeTruthy();
  });

  test('properly iterates to the fourth step (without new moves), and have no cells left', () => {
    grid.next([]);
    expect(grid.cells.length === 0).toBeTruthy();
  });

  test('no new cells are made when no flipping is done on an empty grid', () => {
    grid.next([]);
    expect(grid.cells.length === 0).toBeTruthy();
  });
});
