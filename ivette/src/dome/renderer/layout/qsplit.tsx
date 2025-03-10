/* ************************************************************************ */
/*                                                                          */
/*   This file is part of Frama-C.                                          */
/*                                                                          */
/*   Copyright (C) 2007-2025                                                */
/*     CEA (Commissariat à l'énergie atomique et aux énergies               */
/*          alternatives)                                                   */
/*                                                                          */
/*   you can redistribute it and/or modify it under the terms of the GNU    */
/*   Lesser General Public License as published by the Free Software        */
/*   Foundation, version 2.1.                                               */
/*                                                                          */
/*   It is distributed in the hope that it will be useful,                  */
/*   but WITHOUT ANY WARRANTY; without even the implied warranty of         */
/*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          */
/*   GNU Lesser General Public License for more details.                    */
/*                                                                          */
/*   See the GNU Lesser General Public License version 2.1                  */
/*   for more details (enclosed in the file licenses/LGPLv2.1).             */
/*                                                                          */
/* ************************************************************************ */

/* -------------------------------------------------------------------------- */
/* --- Quarter-based Splitter                                             --- */
/* -------------------------------------------------------------------------- */

/**
    @packageDocumentation
    @module dome/layout/qsplit
*/

import * as React from 'react';
import * as Utils from 'dome/misc/utils';
import { DraggableCore, DraggableEventHandler } from 'react-draggable';
import AutoSizer, { Size } from 'react-virtualized-auto-sizer';

/* -------------------------------------------------------------------------- */
/* --- Q-Split Properties                                                 --- */
/* -------------------------------------------------------------------------- */

export interface QSplitProps {
  /** Q-Split additional class. */
  className?: string;
  /** Q-Split additional style. */
  style?: React.CSSProperties;
  /** Q-Pane to layout in A-quarter. */
  A?: string;
  /** Q-Pane to layout in B-quarter. */
  B?: string;
  /** Q-Pane to layout in C-quarter. */
  C?: string;
  /** Q-Pane to layout in D-quarter. */
  D?: string;
  /** Horizontal top panes ratio (range `0..1`, default `0.5`). */
  HTOP?: number;
  /** Horizontal bottom panes ratio (range `0..1`, default `0.5`). */
  HBOTTOM?: number;
  /** Vertical panes ratio (range `0..1`, default `0.5`). */
  V?: number;
  /** Dragging ratios callback. */
  setPosition?: (HTOP: number, HBOTTOM: number, V: number) => void;
  /** Q-Split contents. Shall be (possibly packed) Q-Panes.
     Other components would be layout as they are in the
     positionned `<div/>` of the Q-Split. */
  children?: React.ReactNode;
}

/* -------------------------------------------------------------------------- */
/* --- Split Bars                                                         --- */
/* -------------------------------------------------------------------------- */

type DragPos = { position: number, anchor: number, offset: number };
type Dragging = undefined | DragPos;

const getDragPosition =
  (d: DragPos): number => d.position + d.offset - d.anchor;

interface BSplitterProps {
  hsplit: boolean;
  style: React.CSSProperties;
  dragging: Dragging;
  setDragging: (dragging: Dragging) => void;
  setPosition: (P: number) => void;
  resetPosition: () => void;
}

const HPOS = 'dome-xSplitter-hpos-R';
const VPOS = 'dome-xSplitter-vpos-R';
const HVPOS = 'dome-xSplitter-hvpos';
const HANDLE = '.dome-xSplitter-grab';
const HGRAB = 'dome-xSplitter-grab dome-xSplitter-hgrab';
const VGRAB = 'dome-xSplitter-grab dome-xSplitter-vgrab';
const HVGRAB = 'dome-xSplitter-grab dome-xSplitter-hvgrab';
const DRAGGING = 'dome-color-dragging';
const DRAGZONE = 'dome-color-dragzone';

function BSplitter(props: BSplitterProps): JSX.Element {
  const { hsplit, style, dragging } = props;

  const onStart: DraggableEventHandler =
    (_evt, data) => {
      const startPos = hsplit ? data.node.offsetLeft : data.node.offsetTop;
      const anchor = hsplit ? data.x : data.y;
      props.setDragging({ position: startPos, offset: anchor, anchor });
    };

  const onDrag: DraggableEventHandler =
    (_evt, data) => {
      if (dragging) {
        const offset = hsplit ? data.x : data.y;
        props.setDragging({ ...dragging, offset });
      }
    };

  const onStop: DraggableEventHandler =
    (evt, _data) => {
      if (evt.metaKey || evt.altKey || evt.ctrlKey) {
        props.resetPosition();
      } else if (dragging) {
        props.setPosition(getDragPosition(dragging));
      }
      props.setDragging(undefined);
    };

  const dragger = Utils.classes(
    hsplit ? HGRAB : VGRAB,
    dragging ? DRAGGING : DRAGZONE,
  );

  const css = hsplit ? HPOS : VPOS;

  return (
    <DraggableCore
      handle={HANDLE}
      onStart={onStart}
      onDrag={onDrag}
      onStop={onStop}
    >
      <div
        className={css}
        style={style}
      >
        <div className={dragger} />
      </div>
    </DraggableCore>
  );
}

/* -------------------------------------------------------------------------- */
/* --- Split Node                                                         --- */
/* -------------------------------------------------------------------------- */

interface CSplitterProps {
  style: React.CSSProperties;
  dragX: Dragging;
  dragY: Dragging;
  setDragX: (dx: Dragging) => void;
  setDragY: (dy: Dragging) => void;
  resetPosition: () => void;
  setPosition: (X: number, Y: number) => void;
}

function CSplitter(props: CSplitterProps): JSX.Element {
  const { style, dragX, dragY } = props;

  const onStart: DraggableEventHandler =
    (_evt, data) => {
      const startX = data.node.offsetLeft;
      const startY = data.node.offsetTop;
      const anchorX = data.x;
      const anchorY = data.y;
      props.setDragX({ position: startX, offset: anchorX, anchor: anchorX });
      props.setDragY({ position: startY, offset: anchorY, anchor: anchorY });
    };

  const onDrag: DraggableEventHandler =
    (_evt, data) => {
      if (dragX) props.setDragX({ ...dragX, offset: data.x });
      if (dragY) props.setDragY({ ...dragY, offset: data.y });
    };

  const onStop: DraggableEventHandler =
    (evt, _data) => {
      if (evt.metaKey || evt.altKey || evt.ctrlKey) {
        props.resetPosition();
      } else if (dragX && dragY) {
        const X = getDragPosition(dragX);
        const Y = getDragPosition(dragY);
        props.setPosition(X, Y);
      }
      props.setDragX(undefined);
      props.setDragY(undefined);
    };

  const dragging = dragX !== undefined && dragY !== undefined;
  const dragger = Utils.classes(HVGRAB, dragging ? DRAGGING : DRAGZONE);
  return (
    <DraggableCore
      handle={HANDLE}
      onStart={onStart}
      onDrag={onDrag}
      onStop={onStop}
    >
      <div
        className={HVPOS}
        style={style}
      >
        <div className={dragger} />
      </div>
    </DraggableCore>
  );
}

/* -------------------------------------------------------------------------- */
/* --- Q-Split Engine                                                     --- */
/* -------------------------------------------------------------------------- */

type QSplitLayout = Map<string, React.CSSProperties>;
const QSplitContext = React.createContext<QSplitLayout>(new Map());
const NODISPLAY: React.CSSProperties = { display: 'none' };

const HSPLIT = (
  left: number,
  top: number,
  height: number,
): React.CSSProperties => ({ display: 'block', left, top, height });

const VSPLIT = (
  left: number,
  top: number,
  width: number,
): React.CSSProperties => ({ display: 'block', left, top, width });

const DISPLAY = (
  layout: QSplitLayout,
  id: string | undefined,
  left: number,
  width: number,
  top: number,
  height: number,
): void => {
  if (id) layout.set(id, { display: 'block', left, width, top, height });
};

interface QSplitEngineProps extends QSplitProps { size: Size }

const inRange = (P: number, D: number): number => Math.max(0, Math.min(P, D));

const getRatio = (P: number, D: number): number => inRange(P, D) / D;

const getPosition = (d: Dragging, D: number, R: number): number =>
  d ? inRange(getDragPosition(d), D) : Math.round(D * R);

type Pid = string | undefined;

const sameOf = (P: Pid, Q: Pid): Pid => {
  if (P === Q) return P;
  else return undefined;
};

const fullOf = (A: Pid, B: Pid, C: Pid, D: Pid): Pid => {
  if (A === B && B === C && C === D) return A;
  else return undefined;
};

function QSplitEngine(props: QSplitEngineProps): JSX.Element {
  const [dragXTop, setDragXTop] = React.useState<Dragging>();
  const [dragXBottom, setDragXBottom] = React.useState<Dragging>();
  const [dragY, setDragY] = React.useState<Dragging>();
  const layout: QSplitLayout = new Map();
  let hsplitTop: React.CSSProperties = NODISPLAY;
  let hsplitBottom: React.CSSProperties = NODISPLAY;
  let vsplit: React.CSSProperties = NODISPLAY;
  let hvsplittop: React.CSSProperties = NODISPLAY;
  let hvsplitbottom: React.CSSProperties = NODISPLAY;

  const { A, B, C, D,
    HTOP = 0.5, HBOTTOM = 0.5, V = 0.5, size, setPosition } = props;
  const { width, height } = size;

  const maxGap = 0.005; // 0.5% max
  function isSmallGap(a: number, b: number): boolean {
    return (a < b && a > (b - maxGap)) || (a > b && a < (b + maxGap));
  }

  const setXTop = React.useCallback((X: number) => {
    if (setPosition) {
      const top = getRatio(X, width);
      const bottom = isSmallGap(top, HBOTTOM) ? top : HBOTTOM;
      setPosition(top, bottom, V);
    }
  }, [setPosition, width, HBOTTOM, V]);
  const setXBottom = React.useCallback((X: number) => {
    if (setPosition) {
      const bottom = getRatio(X, width);
      const top = isSmallGap(bottom, HTOP) ? bottom : HTOP;
      setPosition(top, bottom, V);
    }
  }, [setPosition, width, HTOP, V]);
  const setY = React.useCallback((Y: number) => {
    if (setPosition) setPosition(HTOP, HBOTTOM, getRatio(Y, height));
  }, [setPosition, height, HTOP, HBOTTOM]);
  const setXYTop = React.useCallback((X: number, Y: number) => {
    if (setPosition) {
      const top = getRatio(X, width);
      const bottom = isSmallGap(top, HBOTTOM) ? top : HBOTTOM;
      setPosition(top, bottom, getRatio(Y, height));
    }
  }, [setPosition, HBOTTOM, width, height]);
  const setXYBottom = React.useCallback((X: number, Y: number) => {
    if (setPosition) {
      const bottom = getRatio(X, width);
      const top = isSmallGap(bottom, HTOP) ? bottom : HTOP;
      setPosition(top, bottom, getRatio(Y, height));
    }
  }, [setPosition, HTOP, width, height]);
  const setXY = React.useCallback((X: number, Y: number) => {
    if (setPosition) {
      const x = getRatio(X, width);
      setPosition(x, x, getRatio(Y, height));
    }
  }, [setPosition, width, height]);
  const resetXTop = React.useCallback(() => {
    if (setPosition) setPosition(0.5, 0.5, V);
  }, [setPosition, V]);
  const resetXBottom = React.useCallback(() => {
    if (setPosition) setPosition(0.5, 0.5, V);
  }, [setPosition, V]);
  const resetY = React.useCallback(() => {
    if (setPosition) setPosition(HTOP, HBOTTOM, 0.5);
  }, [setPosition, HTOP, HBOTTOM]);
  const resetXY = React.useCallback(() => {
    if (setPosition) setPosition(0.5, 0.5, 0.5);
  }, [setPosition]);

  const XTop = getPosition(dragXTop, width, HTOP);
  const XBottom = getPosition(dragXBottom, width, HBOTTOM);
  const Y = getPosition(dragY, height, V);
  const RXTop = width - XTop - 1;
  const RXBottom = width - XBottom - 1;
  const RY = height - Y - 1;
  const AB = sameOf(A, B);
  const AC = sameOf(A, C);
  const BD = sameOf(B, D);
  const CD = sameOf(C, D);
  const ABCD = fullOf(A, B, C, D);
  // ----------------------------------------
  // [ ABCD ]
  // ---------------------------------------
  if (ABCD) {
    DISPLAY(layout, ABCD, 0, width, 0, height);
  }
  // ----------------------------------------
  // [ AB -- CD ]
  // ---------------------------------------
  else if (AB && CD) {
    vsplit = VSPLIT(0, Y, width);
    DISPLAY(layout, CD, 0, width, Y + 1, RY);
    DISPLAY(layout, AB, 0, width, 0, Y);
  }
  // ----------------------------------------
  // [ AC | BD ]
  // ---------------------------------------
  else if (AC && BD) {
    hsplitTop = HSPLIT(XTop, 0, height);
    DISPLAY(layout, BD, XTop + 1, RXTop, 0, height);
    DISPLAY(layout, AC, 0, XTop, 0, height);
  }
  // ----------------------------------------
  // [ AB -- C | D ]
  // ----------------------------------------
  else if (AB) {
    hsplitTop = HSPLIT(XTop, Y, RY);
    vsplit = VSPLIT(0, Y, width);
    DISPLAY(layout, D, XTop + 1, RXTop, Y + 1, RY);
    DISPLAY(layout, C, 0, XTop, Y + 1, RY);
    DISPLAY(layout, AB, 0, width, 0, Y);
  }
  // ----------------------------------------
  // [ AC | B -- D ]
  // ----------------------------------------
  else if (AC) {
    hsplitTop = HSPLIT(XTop, 0, height);
    vsplit = VSPLIT(XTop, Y, RY);
    DISPLAY(layout, D, XTop + 1, RXTop, Y + 1, RY);
    DISPLAY(layout, B, XTop + 1, RXTop, 0, Y);
    DISPLAY(layout, AC, 0, XTop, 0, height);
  }
  // ----------------------------------------
  // [ A -- C | BD ]
  // ----------------------------------------
  else if (BD) {
    hsplitTop = HSPLIT(XTop, 0, height);
    vsplit = VSPLIT(0, Y, XTop);
    DISPLAY(layout, C, 0, XTop, Y + 1, RY);
    DISPLAY(layout, A, 0, XTop, 0, Y);
    DISPLAY(layout, BD, XTop + 1, RXTop, 0, height);
  }
  // ----------------------------------------
  // [ A | B -- CD ]
  // ----------------------------------------
  else if (CD) {
    hsplitTop = HSPLIT(XTop, 0, Y);
    vsplit = VSPLIT(0, Y, width);
    DISPLAY(layout, B, XTop + 1, RXTop, 0, Y);
    DISPLAY(layout, A, 0, XTop, 0, Y);
    DISPLAY(layout, CD, 0, width, Y + 1, RY);
  }
  // ----------------------------------------
  // [ A, B, C, D ]
  // ----------------------------------------
  else {
    hsplitTop = HSPLIT(XTop, 0, Y);
    hsplitBottom = HSPLIT(XBottom, Y + 1, height - Y);
    vsplit = VSPLIT(0, Y, width);
    DISPLAY(layout, D, XBottom + 1, RXBottom, Y + 1, RY);
    DISPLAY(layout, C, 0, XBottom, Y + 1, RY);
    DISPLAY(layout, B, XTop + 1, RXTop, 0, Y);
    DISPLAY(layout, A, 0, XTop, 0, Y);
  }
  // ----------------------------------------
  if (hsplitTop !== NODISPLAY)
    hvsplittop = { display: 'block', left: XTop, top: Y };

  if (hsplitBottom !== NODISPLAY && vsplit !== NODISPLAY)
    hvsplitbottom = { display: 'block', left: XBottom, top: Y };

  /** CSplitter HVSPLIT should only be displayed if XTop === XBottom and if
    * all or none of the splitters slide.
   */
  const noDragging = Boolean(!(dragXTop || dragXBottom || dragY));
  const allDragging = Boolean(dragXTop && dragXBottom && dragY);
  const csplitter = Boolean(!hsplitBottom ||
    (XTop === XBottom && (noDragging || allDragging))
  );

  // ----------------------------------------
  // Rendering
  // ----------------------------------------
  return (
    <QSplitContext.Provider value={layout}>
      <BSplitter
        key='HSPLITTOP'
        hsplit={true}
        dragging={dragXTop}
        setDragging={setDragXTop}
        setPosition={setXTop}
        resetPosition={resetXTop}
        style={hsplitTop}
      />
      <BSplitter
        key='HSPLITBOTTOM'
        hsplit={true}
        dragging={dragXBottom}
        setDragging={setDragXBottom}
        setPosition={setXBottom}
        resetPosition={resetXBottom}
        style={hsplitBottom}
      />
      <BSplitter
        key='VSPLIT'
        hsplit={false}
        dragging={dragY}
        setDragging={setDragY}
        setPosition={setY}
        resetPosition={resetY}
        style={vsplit}
      />
      { csplitter ?
        <CSplitter
          key='HVSPLITOP'
          dragX={dragXTop}
          dragY={dragY}
          setDragX={(v: Dragging) => {
            setDragXTop(v);
            setDragXBottom(v);
          }}
          setDragY={setDragY}
          setPosition={setXY}
          resetPosition={resetXY}
          style={hvsplittop}
        /> :
        <>
          <CSplitter
            key='HVSPLITTOP'
            dragX={dragXTop}
            dragY={dragY}
            setDragX={setDragXTop}
            setDragY={setDragY}
            setPosition={setXYTop}
            resetPosition={resetXY}
            style={hvsplittop}
            />
          <CSplitter
            key='HVSPLITBOTTOM'
            dragX={dragXBottom}
            dragY={dragY}
            setDragX={setDragXBottom}
            setDragY={setDragY}
            setPosition={setXYBottom}
            resetPosition={resetXY}
            style={hvsplitbottom}
          />
        </>
      }
      {props.children}
    </QSplitContext.Provider>
  );
}

/* -------------------------------------------------------------------------- */
/* --- Q-Split                                                            --- */
/* -------------------------------------------------------------------------- */

/** Q-Spliiter Container.

   The contained is divided into four quarters named `A`, `B`, `C` and `D`
   with the following layout:

   ```
     A | B
     -----
     C | D
   ```

   The horizontal and vertical split bars can be dragged to adjust the ratios.
   The central node can also be dragged to adust both ratios.

   Any adjacent quarters collapse when they contain either the same component
   or one component and `undefined`. The split bars are erased accordingly.

   When all quarters contain the same component or `undefined`, they all
   collapse and the only component extends to the full container size.

   Other cases are a bit degenerated and lead to « incomplete » layout.
   For instance, when a given component is positionned into two diagonal
   corners but the adjacent quarters can not collapse,
   it will be positionned into only one quarter.
 */
export function QSplit(props: QSplitProps): JSX.Element {
  const CONTAINER = Utils.classes('dome-xSplitter-container', props.className);
  return (
    <div className={CONTAINER} style={props.style}>
      <AutoSizer>
        {(size: Size) => (
          <QSplitEngine size={size} {...props} />
        )}
      </AutoSizer>
    </div>
  );
}

/* -------------------------------------------------------------------------- */
/* --- Q-Pane                                                             --- */
/* -------------------------------------------------------------------------- */

export interface QPaneProps {
  id: string; /** Q-Pane Identifer. */
  className?: string; /** Additional class of the Q-Pane div. */
  style?: React.CSSProperties; /** Additional style of the Q-Pane div. */
  children?: React.ReactNode; /** Q-Pane contents. */
}

/**
   Q-Splitter Components.

   Childrens are rendered in a positionned `<div/>` with absolute coordinates.
 */
export function QPane(props: QPaneProps): JSX.Element {
  const layout = React.useContext(QSplitContext);
  const QPANE = Utils.classes('dome-xSplitter-pane', props.className);
  const QSTYLE = Utils.styles(props.style, layout?.get(props.id) ?? NODISPLAY);
  return <div className={QPANE} style={QSTYLE}>{props.children}</div>;
}

// --------------------------------------------------------------------------
