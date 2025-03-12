import type { Colourscheme } from "$lib/colorscheme";

export interface InitData<T> {
  type: 'init',
  init: T;
}

export type InitMessage = {
  canvas: OffscreenCanvas;
  canvasWidth: number;
  canvasHeight: number;
  storePrefix: string;
  totalFrames: number;
  colourscheme: Colourscheme;
};

export interface PagingData<T> {
  type: 'page',
  page: T;
}

export type Data = InitData<InitMessage> | PagingData<number>;
