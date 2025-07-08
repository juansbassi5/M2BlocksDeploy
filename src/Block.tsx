import { motion } from 'framer-motion';
import { numberToColor } from './util';

export type Position = [number, number];

interface BlockProps {
  value: number;
  position: Position;
  /** si es true, no hacemos la animación de lanzamiento */
  skipLaunch?: boolean;
}

function Block({ value, position, skipLaunch = false }: BlockProps) {
  const [row, column] = position;
  const launchOffset = 200;

  return (
    <motion.div
      className="block"
      style={{
        backgroundColor: numberToColor(value),
        gridRow: row + 1,
        gridColumn: column + 1
      }}
      initial={
        skipLaunch
          ? false
          : { y: launchOffset, scale: 0.8, opacity: 0.8 }
      }
      animate={{ y: 0, scale: 1, opacity: 1 }}
      transition={{ duration: 0.25, ease: 'easeOut' }}
    >
      {value}
    </motion.div>
  );
}

export default Block;