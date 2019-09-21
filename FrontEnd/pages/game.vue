<template id="salz-game-view">
  <div id="salz-game-inner-view"></div>
</template>

<style lang="scss">
#salz-game-inner-view {
  height: 100vh;
}
</style>

<script charset="utf-8">
import { fullscreen } from '../lib/game-rendering/fullscreen';
import Frame from '../lib/game-rendering/frame';

// This is a dummy data set
import { importedData } from '../lib/game-rendering/testData';

export default {
  data() {
    return {
      index: []
    };
  },
  // async asyncData(ctx) {
  //   return {
  //     // index: await ctx.app.$framesRepo.index()
  //   };
  // },
  async mounted() {
    const PIXI = await import('pixi.js');
    const Viewport = await import('pixi-viewport');
    // this.index.frames.forEach((frame) => {
    //   console.log(frame['1']);
    // });

    const wrapper = document.querySelector('#salz-game-inner-view');

    const appWidth = Math.max(
      document.documentElement.clientWidth,
      window.innerWidth || 0
    );
    const appHeight = Math.max(
      document.documentElement.clientHeight,
      window.innerHeight || 0
    );

    const app = new PIXI.Application({
      width: appWidth,
      height: appHeight,
      backgroundColor: 0x000000,
      resizeTo: document.querySelector('.main-content'),
      resolution: window.devicePixelRatio || 1
    });
    wrapper.appendChild(app.view);

    const vpWorldWidth = 1000;
    const vpWorldHeight = 1000;

    const viewport = new Viewport.Viewport({
      screenWidth: window.innerWidth,
      screenHeight: window.innerHeight,
      worldWidth: vpWorldWidth,
      worldHeight: vpWorldHeight,
      interaction: app.renderer.plugins.interaction
    });
    viewport.moveCenter(0, 0);
    app.stage.addChild(viewport);
    viewport
      .drag()
      .pinch()
      .wheel()
      .decelerate()
      .clampZoom({
        minWidth: vpWorldWidth / 10,
        minHeight: vpWorldHeight / 10,
        maxWidth: vpWorldWidth * 10,
        maxHeight: vpWorldHeight * 10
      });

    // const btn = createTextButton(
    //   'Fullscreen',
    //   { padding: 10, fill: '#FF0000' },
    //   () => {
    //     fullscreen(app.view);
    //   }
    // );
    // btn.x = appWidth - btn.width - 20;
    // btn.y = appHeight - btn.height - 20;

    let curFrame = 0;
    const maxFrame = importedData.length;
    let frame = new Frame(viewport, importedData[0]);
    frame.draw();

    // hotkeys
    document.onkeydown = function(e) {
      switch (e.keyCode) {
        case 70: // f
          fullscreen(app.view);
          break;
        case 72: // h
          if (curFrame > 0) {
            viewport.removeChildren(0, viewport.length);
            frame = new Frame(viewport, importedData[--curFrame]);
            frame.draw();
          }
          break;
        case 76: // l
          if (curFrame < maxFrame - 1) {
            viewport.removeChildren(0, viewport.length);
            frame = new Frame(viewport, importedData[++curFrame]);
            frame.draw();
          }
          break;
        case 37: // left
          break;
        case 39: // right
          break;
      }
    };

    // to be refactored so that it can be used with image assets
    // function createTextButton(text, style, fn) {
    //   const btn = new PIXI.Text(text, style);
    //   btn.interactive = true;
    //   btn.buttonMode = true;
    //   btn.on('click', fn);
    //   app.stage.addChild(btn);

    //   return btn;
    // }

    window.addEventListener('resize', (event) => {
      app.resize();

      // const appWidth = Math.max(
      //   document.documentElement.clientWidth,
      //   window.innerWidth || 0
      // );
      // const appHeight = Math.max(
      //   document.documentElement.clientHeight,
      //   window.innerHeight || 0
      // );
      // btn.x = appWidth - btn.width - 20;
      // btn.y = appHeight - btn.height - 20;
    });
  }
};
</script>
