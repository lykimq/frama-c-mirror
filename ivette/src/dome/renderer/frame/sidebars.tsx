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

// --------------------------------------------------------------------------
// --- SideBars
// --------------------------------------------------------------------------

/**
   @packageDocumentation
   @module dome/frame/sidebars
*/

import React from 'react';
import { useFlipSettings } from 'dome';
import { Badge } from 'dome/controls/icons';
import { Label, Title } from 'dome/controls/labels';
import { classes } from 'dome/misc/utils';
import { Hbox } from 'dome/layout/boxes';
import { IconButton, IconButtonProps } from 'dome/controls/buttons';

import './style.css';

// --------------------------------------------------------------------------
// --- SideBar Container
// --------------------------------------------------------------------------

export interface SideBarProps {
  className?: string;
  style?: React.CSSProperties;
  children?: React.ReactNode;
}

/**
   Container for sidebar items.
 */
export function SideBar(props: SideBarProps): JSX.Element {
  const className = classes(
    'dome-xSideBar',
    'dome-color-frame',
    props.className,
  );
  return (
    <div className={className} style={props.style}>
      {props.children}
    </div>
  );
}

// --------------------------------------------------------------------------
// --- Badges Specifications
// --------------------------------------------------------------------------

export type BadgeElt = undefined | null | string | number | React.ReactNode;
export type Badges = BadgeElt | BadgeElt[];

const makeBadgeElt = (elt: BadgeElt, index: number): React.ReactNode => {
  if (elt === undefined || elt === null) return null;
  switch (typeof (elt)) {
    case 'number':
    case 'string':
      return <Badge value={elt} key={`item#${index}`} />;
    default:
      return elt;
  }
};

const makeBadge = (elt: Badges): React.ReactNode => {
  if (Array.isArray(elt))
    return elt.map(makeBadgeElt);
  return makeBadgeElt(elt, 0);
};

// --------------------------------------------------------------------------
// --- SideBar Section
// --------------------------------------------------------------------------

export interface SectionProps {
  /** Section label. */
  label: string;
  /** Section tooltip description. */
  title?: string;
  /** Hide/Show window settings. */
  settings?: string;
  /** Controlled Fold/Unfold state. */
  unfold?: boolean;
  /** Initial unfold state (default is `true`). */
  defaultUnfold?: boolean;
  /** Enabled sections are made visible. */
  enabled?: boolean;
  /** Disabled sections are made unvisible. */
  disabled?: boolean;
  /** Badge summary (only visible when folded). */
  summary?: Badges;
  /** Additional label, right-aligned. */
  infos?: string;
  /** Additional controls, (only visible when unfolded). */
  rightButtonProps?: IconButtonProps;
  /** Section contents. */
  children?: React.ReactNode;
  /** Additionnal CSS class. */
  className?: string;
}

/**
   Sidebar Section.

   Unless specified, sections can be hidden on click.
   When items in the section have badge(s)
   it is highly recommended to provide a badge summary to be displayed
   when the content is hidden.

   Sections with no items are not displayed.
*/
export function Section(props: SectionProps): JSX.Element | null {
  const { settings, defaultUnfold, infos, unfold } = props;
  const [state, flipState] = useFlipSettings(settings, defaultUnfold);
  const icon = state ? 'TRIANGLE.DOWN' : 'TRIANGLE.RIGHT';

  const { enabled = true, disabled = false, children } = props;
  if (disabled || !enabled || React.Children.count(children) === 0) return null;

  const visible = unfold ?? state;
  const maxHeight = visible ? 'max-content' : 0;
  const { rightButtonProps: iconProps } = props;
  const className = `dome-xSideBarSection-filterButton ${iconProps?.className}`;
  const rightButton =
    iconProps ? <IconButton {...iconProps} className={className}/> : undefined;

  return (
    <div className={`dome-xSideBarSection ${props.className}`}>
      <Hbox className='dome-xSideBarSection-title' >
        <Label
          title={props.title}
          label={props.label}
          icon={icon}
          onClick={flipState}
        />
        {infos && <div className='dome-xSideBarSection-infos'>{infos}</div>}
        {visible ? rightButton : makeBadge(props.summary)}
      </Hbox>
      <div className='dome-xSideBarSection-content' style={{ maxHeight }}>
        {children}
      </div>
    </div>
  );
}

// --------------------------------------------------------------------------
// --- SideBar Items
// --------------------------------------------------------------------------

export interface ItemProps {
  /** Item icon. */
  icon?: string;
  /** Item label. */
  label?: string;
  /** Item tooltip text. */
  title?: string;
  /** Badge. */
  badge?: Badges;
  /** Enabled item. */
  enabled?: boolean;
  /** Disabled item (dimmed). */
  disabled?: boolean;
  /** Selection state. */
  selected?: boolean;
  /** Selection callback. */
  onSelection ?: (e: React.MouseEvent) => void
  /** Double-click callback. */
  onDoubleClick ?: (e: React.MouseEvent) => void
  /** Right-click callback. */
  onContextMenu?: (e: React.MouseEvent) => void;
  /** Additional class. */
  className?: string;
  /** Additional styles. */
  style?: React.CSSProperties;
  /** Other item elements. */
  children?: React.ReactNode;
}

/** Sidebar Items. */
export function Item(props: ItemProps): JSX.Element {
  const { selected = false, disabled = false, enabled = true } = props;
  const isDisabled = disabled || !enabled;
  const ref = React.useRef<HTMLDivElement>(null);
  const [clicked, setClicked] = React.useState(false);

  const fnClick = isDisabled ? undefined : props.onSelection;
  const onClick =
    fnClick ? (e: React.MouseEvent) => {
      setClicked(true);
      fnClick(e);
    } : undefined;

  const fnDoubleClick = isDisabled ? undefined : props.onDoubleClick;
  const onDoubleClick =
    fnDoubleClick ? (e: React.MouseEvent) => {
      setClicked(true);
      fnDoubleClick(e);
    } : undefined;

  const fnContextMenu = isDisabled ? undefined : props.onContextMenu;
  const onContextMenu =
    fnContextMenu ? (e: React.MouseEvent) => {
      setClicked(true);
      fnContextMenu(e);
    } : undefined;

  const className = classes(
    'dome-xSideBarItem',
    selected ? 'dome-active' : 'dome-inactive',
    isDisabled && 'dome-disabled',
    props.className,
  );

  React.useLayoutEffect(() => {
    if (!clicked && selected) {
      ref?.current?.scrollIntoView({
        behavior: 'auto',
        inline: 'nearest',
      });
    }
    if (!selected && clicked)
      setClicked(false);
  }, [clicked, selected]);

  return (
    <div
      ref={ref}
      className={className}
      style={props.style}
      title={props.title}
      onClick={onClick}
      onDoubleClick={onDoubleClick}
      onContextMenu={onContextMenu}
    >
      <Label icon={props.icon} label={props.label} />
      {props.children}
      {makeBadge(props.badge)}
    </div>
  );
}

// --------------------------------------------------------------------------
// --- SideBar Title
// --------------------------------------------------------------------------

export interface SidebarTitleProps {
  /** Label. */
  label: string;
  /** Additionnal CSS class. */
  className?: string;
  /** Other elements. */
  children?: React.ReactNode;
}

/** Sidebar Title. */
export function SidebarTitle(props: SidebarTitleProps): JSX.Element {
  const { label, children } = props;
  const className = classes(
    'dome-xSideBarTitle',
    props.className,
  );
  return (
    <Hbox className={className}>
      <Hbox><Title label={label} title={label}/></Hbox>
      { children}
    </Hbox>
  );
}
// --------------------------------------------------------------------------
