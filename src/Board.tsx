import { motion } from 'framer-motion';
import Block, { Position } from './Block';
import { Grid } from './Game';

interface BoardProps {
  grid: Grid;
  numOfColumns: number;
  onLaneClick: (lane: number) => void;
  fusionGroup: number[];
  fusionReceptorIndex?: number; // NEW: optional fusionReceptorIndex prop
  hoveredLane: number | null;
  setHoveredLane: (lane: number | null) => void;
}

function Board({ grid, numOfColumns, onLaneClick, fusionGroup, fusionReceptorIndex, hoveredLane, setHoveredLane }: BoardProps) {
  const numOfRows = grid.length / numOfColumns;

  let receptorOffset = { x: 0, y: 0 };
  if (fusionGroup.length > 1 && fusionReceptorIndex !== undefined) { // Check fusionReceptorIndex
    const receptorIdx = fusionReceptorIndex; // Use the passed receptor index
    const receptorRow = Math.floor(receptorIdx / numOfColumns);
    const receptorCol = receptorIdx % numOfColumns;

    let sumX = 0, sumY = 0;
    // Only calculate offset for blocks that are part of the fusion group and not the receptor
    fusionGroup.filter(idx => idx !== receptorIdx).forEach(neighborIdx => {
      const nRow = Math.floor(neighborIdx / numOfColumns);
      const nCol = neighborIdx % numOfColumns;
      sumX += (nCol - receptorCol);
      sumY += (nRow - receptorRow);
    });

    receptorOffset = {
      x: sumX * 5,
      y: sumY * 5
    };
  }

  return (
    <div className="board">
      <div
        className="blocks"
        style={{
          gridTemplateColumns: `repeat(${numOfColumns}, 70px)`,
          gridTemplateRows: `repeat(${numOfRows}, 70px)`
        }}
      >
        {Array.from({ length: numOfColumns }).map((_, i) => {
          const isHovered = hoveredLane === i + 1;

          return (
            <div
              key={`lane-${i}`}
              className="lane"
              style={{
                gridColumn: i + 1,
                gridRow: `1 / span ${numOfRows}`,
                position: 'relative',
                zIndex: 1
              }}
              onClick={() => onLaneClick(i + 1)}
              onMouseEnter={() => setHoveredLane(i + 1)}
              onMouseLeave={() => setHoveredLane(null)}
            >
            </div>
          );
        })}

        {grid.map((num, i) => {
          if (num === "-") return null;
          const row = Math.floor(i / numOfColumns), col = i % numOfColumns;
          const pos: Position = [row, col];

          const isInFusion = fusionGroup.includes(i);
          // NEW: Check if the current block is the designated receptor
          const isReceptor = fusionReceptorIndex !== undefined && i === fusionReceptorIndex;

          return (
            <motion.div
              key={`${row}-${col}`} // Use a more stable key for motion.div if possible
              initial={false} // Prevents initial animation from 0,0
              animate={
                isInFusion
                  ? (
                    isReceptor
                      ? { scale: 1.1, x: receptorOffset.x, y: receptorOffset.y } // Apply offset to receptor
                      : {
                        x: (fusionReceptorIndex! % numOfColumns - col) * 20, // Animate towards receptor
                        y: (Math.floor(fusionReceptorIndex! / numOfColumns) - row) * 20, // Animate towards receptor
                        scale: 0,
                        opacity: 0
                      }
                  )
                  : { scale: 1, x: 0, y: 0, opacity: 1 } // Reset for non-fusion blocks
              }
              transition={{ duration: 0.4, ease: "easeOut" }}
              style={{
                gridRow: row + 1,
                gridColumn: col + 1,
                zIndex: isReceptor ? 2 : 1,
                boxShadow: isReceptor ? '0 0 10px gold' : undefined,
                border: isReceptor ? '2px solid gold' : undefined,
                borderRadius: '8px',
                position: 'relative'
              }}
            >
              <Block
                value={Number(num)}
                position={pos}
                skipLaunch={isReceptor} // Only the receptor skips the launch animation if it's the target
              />
            </motion.div>
          );
        })}
      </div>
    </div>
  );
}

export default Board;