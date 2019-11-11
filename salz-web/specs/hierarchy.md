# Sketchy Hierarchy of Classes for Salz

This is meant to provide a high level overview of the structure
of `pages/game.vue`.

```
App
|- Viewport
   |- GameFrame
      |- Frame
         |- Players
            |- Cells
```

1. Viewport
  * There is only 1 Viewport
  * Child: one and only one GameFrame

2. GameFrame
  * There is only 1 GameFrame
  * Extends PIXI.Container
  * Child: Frame
  * "Sub"-child for rendering: Cells

3. Frame
  * A Frame has an array of Players

4. Player
  * A Player has an array of Cells
