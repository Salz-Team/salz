<template id="salz-game-view">
  <div id="salz-game-inner-view"></div>
</template>

<style lang="scss">
@import '~assets/css/colors';
#salz-game-inner-view {
  width: 100vw;
  height: 100vh;
}

.gameUIContainer {
  background: var(--body-bg-color);
  border-radius: 2px;
  box-shadow: 2px 2px 5px 5px $black;
}

.cellInfoContainer {
  padding: 20px;
  position: fixed;
}
</style>

<script charset="utf-8">
import hotkeys from 'hotkeys-js';

// Game Rendering
import { fullscreen } from '../lib/game/rendering/fullscreen';
import Frame from '../lib/game/rendering/frame';
import { Color } from '../lib/game/colors.js';

// This is a dummy data set
// import { importedData } from '../lib/game/testData';

export default {
  data() {
    return {
      index: []
    };
  },
  async asyncData(ctx) {
    return {
      index: await ctx.app.$framesRepo.index() // read frame info from api
    };
  },
  async mounted() {
    const PIXI = await import('pixi.js');
    const Viewport = await import('pixi-viewport');
    const wrapper = document.querySelector('#salz-game-inner-view');
    const frames = this.index.frames;

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
      // eslint-disable-next-line
      backgroundColor: Color.black,
      resizeTo: wrapper,
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
    frames.forEach((frame) => {
      frame.forEach((player) => {
        player.color = Color.primary;
      });
    });
    let frame = new Frame(viewport, frames[0]);
    frame.draw();

    function drawLastFrame() {
      if (curFrame > 0) {
        viewport.removeChildren(0, viewport.length);
        document.querySelectorAll('.cellInfoContainer').forEach((c) => {
          c.parentNode.removeChild(c);
        });
        frame = new Frame(viewport, frames[--curFrame]);
        frame.draw();
      }
    }

    function drawNextFrame() {
      if (curFrame < frames.length - 1) {
        viewport.removeChildren(0, viewport.length);
        document.querySelectorAll('.cellInfoContainer').forEach((c) => {
          c.parentNode.removeChild(c);
        });
        frame = new Frame(viewport, frames[++curFrame]);
        frame.draw();
      }
    }

    /**
     * Move viewport camera by dx and dy
     *
     * @param   {number}  dx  Pixels to move x by
     * @param   {number}  dy  Pixels to move y by
     */
    function moveViewport(dx, dy) {
      viewport.moveCenter(viewport.center.x + dx, viewport.center.y + dy);
    }

    // hotkeys
    const gameHotkeys = [
      {
        key: 'f',
        fn: () => {
          fullscreen(wrapper);
        }
      },
      {
        key: 'h',
        fn: () => {
          drawLastFrame();
        }
      },
      {
        key: 'l',
        fn: () => {
          drawNextFrame();
        }
      },
      {
        key: 'z',
        fn: viewport.zoomPercent(0.1)
      },
      {
        key: 'shift+z',
        fn: viewport.zoomPercent(-0.1)
      },
      {
        key: 'left',
        fn: (event) => {
          event.preventDefault();
          moveViewport(-10, 0);
        }
      },
      {
        key: 'shift+left',
        fn: (event) => {
          event.preventDefault();
          moveViewport(-50, 0);
        }
      },
      {
        key: 'up',
        fn: (event) => {
          event.preventDefault();
          moveViewport(0, -10);
        }
      },
      {
        key: 'shift+up',
        fn: (event) => {
          event.preventDefault();
          moveViewport(0, -50);
        }
      },
      {
        key: 'right',
        fn: (event) => {
          event.preventDefault();
          moveViewport(10, 0);
        }
      },
      {
        key: 'shift+right',
        fn: (event) => {
          event.preventDefault();
          moveViewport(50, 0);
        }
      },
      {
        key: 'down',
        fn: (event) => {
          event.preventDefault();
          moveViewport(0, 10);
        }
      },
      {
        key: 'shift+down',
        fn: (event) => {
          event.preventDefault();
          moveViewport(0, 50);
        }
      }
    ];

    // register each hotkey for Hotkey.js
    gameHotkeys.forEach((item) => {
      hotkeys(item.key, item.fn);
    });

    // window.addEventListener('resize', (event) => {
    //   app.resize();

    //   const appWidth = Math.max(
    //     document.documentElement.clientWidth,
    //     window.innerWidth || 0
    //   );
    //   const appHeight = Math.max(
    //     document.documentElement.clientHeight,
    //     window.innerHeight || 0
    //   );
    //   btn.x = appWidth - btn.width - 20;
    //   btn.y = appHeight - btn.height - 20;
    // });
  }
};
</script>
