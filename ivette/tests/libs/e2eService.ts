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

import { ElectronApplication, Page, expect } from "@playwright/test";
import { _electron as electron } from "playwright-core";
import * as locs from "./locatorsUtil";

/**
 * Basic Electron launch of Ivette for Playwright tests
 */
export async function launchIvette(
  ...params: string[]
): Promise<{ app: ElectronApplication; page: Page }> {
  const args: string[] = [
    "./out/main/index.js",
    "--no-sandbox",
    "--settings", "DEFAULT"
  ];
  params.forEach(p => {
    p.trim().split(/\s+/).forEach(a => args.push(a));
  });
  const electronApp = await electron.launch({
    env: {
      ...process.env,
      NODE_ENV: "development",
    },
    args: args,
  });

  // Get the first window that the app opens, wait if necessary
  const window = await electronApp.firstWindow();

  return {
    app: electronApp,
    page: window,
  };
}

export async function testServerIsStarted(window: Page): Promise<void> {
  // Click on the Console tab in the right menu
  await locs.getConsoleView(window).click();

  // Check the server status in the header's button bar
  await expect(locs.getStartServerButton(window)).toBeDisabled();
  await expect(locs.getShutDownServerButton(window)).toBeEnabled();

  // Check the server status in the console view
  await expect(
    locs.getConsoleComponent(window)
      .getByText("[server] Socket server running.")
  ).toBeVisible();

  // Check the server status in the footer
  await expect(locs.getServerStatusLabel(window)).toHaveText("ON");
}

export async function testFileIsLoaded(window: Page, file: string):
  Promise<void> {
  await locs.getConsoleView(window).click();
  // Check if a message is present in the console view to confirm the file is
  // loaded
  await expect(
    locs.getConsoleComponent(window).getByText(`${file} (with preprocessing)`)
  ).toBeVisible();

  // Check if the main function is visible in the functions view
  // does not work: need to click on AST view...
  // await expect(locs.getMainFunction(window)).toBeVisible();
}
