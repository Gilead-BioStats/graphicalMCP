// #include <cpp11.hpp>
//
// // This is a simple example of exporting a C++ function to R. You can
// // source this function into an R session using `cpp11::cpp_source()`.
//
// // Learn more about cpp11 at:
// //
// //   https://cpp11.r-lib.org/index.html
//
//
//
// // Used for indexing into 2d weights/alphas array
// int ind(int row, int col, int nrow){
//   // returns index of a matrix stored
//   return nrow*col+row;
// }
//
// // I think layer in here is which graph you're in when there's multiple graphs?
// // Used for indexing into 3d transitions array
// int aind(int row, int col, int layer, int nrow){
//   // returns index of an array
//   return layer*nrow*nrow + nrow*col+row;
// }
//
// // h - results vector
// // a - alpha * weights (possibly matrix?)
// // G - transitions matrix
// // p - p-values (possibly matrix?)
// // nH - # of hypotheses
// // G1 - temporary transition storage
// // nGraphs - # of graphs
// // print - Boolean, whether to print?
// // upscale - Boolean, whether to upscale weights to 1
// //
// // grId graph index, i index, j index, rej index of rejections,
// // dm nH, sumrej count of rejections, nG nGraphs,
// // asum total alpha for a hypothesis across all graphs, tmp ??
// [[cpp11::register]]
// void graphproc(double* h, double* a, double* G,
//                double* p, int* nH, double* G1,
//                int* nGraphs, int* print, int* upscale){
//   int grId,i,j,rej,dm,sumrej=0,nG;
//   double asum,tmp;
//   dm = *nH;
//   nG = *nGraphs;
//
//   // loop until !rej or all rejected
//   while(1){
//
//     // pick first significant test
//     rej = 0;
//     // iterate over dimension of graph(s)
//     for(i = 0; i < dm; i++) {
//       asum = 0;
//       // iterate over graphs
//       for(grId = 0; grId < nG; grId++) {
//         // asum is the sum of alphas across all graphs for the given hyp index
//         asum += a[ind(grId,i,nG)];
//       }
//       // p-values are just a vector, it looks like. This is checking whether a
//       // single hyp's p-val is less than (weighted) alpha. If so, the current index
//       // gets stored in rej
//       rej = (p[i] < asum)*(i+1); // i + 1 is converting to R-style 1-indexing
//       // increase count of rejections and break infinite while loop
//       if(rej) {
//         sumrej++;
//         break;
//       }
//     }
//
//     if(rej) {
//       rej = rej - 1; // recover index in C indexing
//       h[rej] = 1; // mark hypothesis as rejected
//       if(sumrej == dm){ // all hypotheses rejected
//         break;
//       }
//       // iterate over graphs
//       for(grId = 0; grId < nG; grId++) {
//         // iterate over hypotheses within each graph (for edges, this is the start
//         // node)
//         for(i = 0; i < dm; i++) {
//           // update local alphas
//           // ind() indexes into row/column of a matrix
//           // aind() indexes into row/column of the appropriate matrix element of
//           // a 3d array
//           a[ind(grId,i,nG)] += a[ind(grId,rej,nG)] * G[aind(rej,i,grId,dm)];
//           if(sumrej < dm-1){ // update graph weights (only when there are more than 2 hypotheses left)
//             // iterate over hypotheses in each graph (end node)
//             for(j = 0; j < dm; j++) {
//               if(i == j) {
//                 // a node to itself must be 0
//                 G1[aind(i,j,grId,dm)] = 0;
//               } else {
//                 // numerator of new transition
//                 tmp = (G[aind(i,j,grId,dm)] + G[aind(i,rej,grId,dm)]*G[aind(rej,j,grId,dm)]);
//                 // check denominator won't be <= 0
//                 // why is this in here? can it ever actually happen?
//                 if (G[aind(i,rej,grId,dm)] * G[aind(rej,i,grId,dm)] < 1) {
//                   // update transition value
//                   G1[aind(i,j,grId,dm)] = tmp/(1 - G[aind(i,rej,grId,dm)]*G[aind(rej,i,grId,dm)]);
//                 } else {
//                   G1[aind(i,j,grId,dm)] = 0;
//                 }
//               }
//             }
//           }
//         }
//         // at this point, one graph has had all nodes/edges updated
//
//         // if there are more than 2 hypotheses left, copy G1 values into G
//         // (presumably for the next loop iteration)
//         if(sumrej < dm - 1) {
//           // index into the right graph
//           for(i = dm*dm*grId; i < (grId+1)*dm*dm; i++) {
//             G[i] = G1[i];
//           }
//         } else { // no hypotheses left
//           for(i = dm*dm*grId; i < (grId+1)*dm*dm; i++) {
//             G[i] = 0;
//           }
//         }
//         // correct all nodes/edges touching the rejected node
//         for(i = 0; i < dm; i++) {
//           G[aind(rej,i,grId,dm)] = 0;
//           G[aind(i,rej,grId,dm)] = 0;
//         }
//         a[ind(grId,rej,nG)] = 0;
//       } // end of graph loop - now all graphs are updated to remove this one node
//
//       // break if there are no more to reject (this breaks the infinite loop)
//     } else {
//       break;
//     }
//   }
// }
//
