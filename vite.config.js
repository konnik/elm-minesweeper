import { defineConfig } from "vite";
import elmPlugin from 'vite-plugin-elm'

export default defineConfig({
  base: "/elm-minesweeper",
  plugins: [elmPlugin()],
});
