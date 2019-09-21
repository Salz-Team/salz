<template id="salz-game-view">
  <div id="salz-game-inner-view"></div>
</template>

<style lang="scss">
#salz-game-inner-view {
  height: 100vh;
}
</style>

<script charset="utf-8">
import hotkeys from 'hotkeys-js';

// Game Rendering
import { fullscreen } from '../lib/game/rendering/fullscreen';
import Frame from '../lib/game/rendering/frame';

// This is a dummy data set
import { importedData } from '../lib/game/testData';

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
    viewport.moveCenter(0, 0); // should act differently for auth-ed users
    app.stage.addChild(viewport);
    viewport
      .drag()
      .pinch()
      .wheel()
      .decelerate()
      .clampZoom({
        minWidth: vpWorldWidth / 5,
        minHeight: vpWorldHeight / 5,
        maxWidth: vpWorldWidth * 5,
        maxHeight: vpWorldHeight * 5
      });

    let curFrame = 0;
    const maxFrame = importedData.length;
    let frame = new Frame(viewport, importedData[0]);
    frame.draw();

    // hotkeys
    const gameHotkeys = [
      {
        key: 'f',
        fn: () => {
          fullscreen(app.view);
        }
      },
      {
        key: 'h',
        fn: () => {
          if (curFrame > 0) {
            viewport.removeChildren(0, viewport.length);
            frame = new Frame(viewport, importedData[--curFrame]);
            frame.draw();
          }
        }
      },
      {
        key: 'l',
        fn: () => {
          if (curFrame < maxFrame - 1) {
            viewport.removeChildren(0, viewport.length);
            frame = new Frame(viewport, importedData[++curFrame]);
            frame.draw();
          }
        }
      },
      {
        key: 'z',
        fn: () => {
          viewport.zoomPercent(0.1);
        }
      },
      {
        key: 'shift+z',
        fn: () => {
          viewport.zoomPercent(-0.1);
        }
      },
      {
        key: 'left',
        fn: (event) => {
          event.preventDefault();
          viewport.moveCenter(viewport.center.x - 10, viewport.center.y);
        }
      },
      {
        key: 'shift+left',
        fn: (event) => {
          event.preventDefault();
          viewport.moveCenter(viewport.center.x - 50, viewport.center.y);
        }
      },
      {
        key: 'up',
        fn: (event) => {
          event.preventDefault();
          viewport.moveCenter(viewport.center.x, viewport.center.y - 10);
        }
      },
      {
        key: 'shift+up',
        fn: (event) => {
          event.preventDefault();
          viewport.moveCenter(viewport.center.x, viewport.center.y - 50);
        }
      },
      {
        key: 'right',
        fn: (event) => {
          event.preventDefault();
          viewport.moveCenter(viewport.center.x + 10, viewport.center.y);
        }
      },
      {
        key: 'shift+right',
        fn: (event) => {
          event.preventDefault();
          viewport.moveCenter(viewport.center.x + 50, viewport.center.y);
        }
      },
      {
        key: 'down',
        fn: (event) => {
          event.preventDefault();
          viewport.moveCenter(viewport.center.x, viewport.center.y + 10);
        }
      },
      {
        key: 'shift+down',
        fn: (event) => {
          event.preventDefault();
          viewport.moveCenter(viewport.center.x, viewport.center.y + 50);
        }
      }
    ];

    gameHotkeys.forEach((item) => {
      hotkeys(item.key, item.fn);
    });

    // to be refactored so that it can be used with image assets
    // const btn = createTextButton(
    //   'Fullscreen',
    //   { padding: 10, fill: '#FF0000' },
    //   () => {
    //     fullscreen(app.view);
    //   }
    // );
    // btn.x = appWidth - btn.width - 20;
    // btn.y = appHeight - btn.height - 20;
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
