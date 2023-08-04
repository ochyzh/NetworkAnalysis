#1. Load the `sampson` dataset from the `sna` library.
library(sna)
data(sampson)

#2. How many different roles do you think are in these data?

#3. Estimate an ego ERGM on `samplike` data.

network::list.vertex.attributes(samplike)

m2 <- ego_ergm(net = samplike,
               form = c("edges", "mutual",
                        'desp(.8, type="OSP")',
                        'desp(.8, type="ISP")'),
               core_size = 1,
               min_size = 5,
               roles = 3,
               forking = FALSE,
               ncpus = 1,
               directed = TRUE,
               edge_covariates = FALSE,
               seed = 12345,
               steps = 50,
               tol = 1e-06)

#4. Cross-tabulate the roles you identified with the `group` vertex attribute. Does it seem like the model is picking up differences based on groups?

table(samplike %v% "group",m2$role_assignments[,2])

#5. Cross-tabulate the roles you identified with the `cloisterville` vertex attribute. Does it seem like the model is picking up differences based on whether monks attended Cloisterville?

table(samplike %v% "cloisterville",m2$role_assignments[,2])

#6. Make a network graph, coloring nodes by role and using node size to show group belonging.


plot(samplike,
        displaylabels = TRUE,
        # size of nodes based on vector vertexSize
        vertex.cex = scales::rescale(degree(samplike), to = c(1,5)),
        # color of nodes based on vertex attribute: group
        #vertex.col = 'group',
     vertex.border = NA,
     label = m2$role_assignments[,2],
     label.pos = 5,
     label.col ="white",
     vertex.col="group",
     edge.col="grey40"
)

