using Google.OrTools.ConstraintSolver;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static IAS.Optimizer.OrTools.FlexibleJobShopData;

namespace IAS.Optimizer.OrTools
{
    public class FlexibleJobShopData
    {
        public class Machine
        {
            public uint Id { get; }
            public string Name { get; }

            public Machine(uint id)
            {
                Id = id;
                Name = $"M_{Id}";
            }
        }

        // A task is the basic block of a jobshop.
        // Each job consists of several tasks that
        // can be performed on a set of machines 
        // with given processing time (durations)
        public class Task
        {
            public uint Id { get; }
            public uint JobId { get; }
            public List<Machine> Machines { get; }
            public List<uint> Durations { get; }
            public uint AlternativesCount { get; }
            public string Name { get; }

            public Task(uint id, uint jobId, List<Machine> machines, List<uint> durations)
            {
                var machinesCount = machines.Count;
                var durationsCount = durations.Count;
                if (machinesCount < 1)
                {
                    throw new ArgumentException("Machines must contain at least one machine!");
                }
                if (durationsCount < 1)
                {
                    throw new ArgumentException("Durations must contain at least one duration!");
                }
                if (machinesCount != durationsCount)
                {
                    throw new ArgumentException("Machines and durations must have the same length!");
                }
                Id = id;
                JobId = jobId;
                Machines = machines;
                Durations = durations;
                AlternativesCount = (uint)machinesCount;
                Name = $"J_{JobId}, T_{Id}, AltCount={AlternativesCount}";
            }
            
            public string DebugString()
            {
                var sb = new StringBuilder($"{Name}: ");
                for (int i = 0; i < AlternativesCount; i++)
                {
                    sb.Append($"({Machines[i].Name},{Durations[i]})");
                    if (i < AlternativesCount-1)
                    {
                        sb.Append(" | ");
                    }
                }
                return sb.ToString();
            }
        }

        public class Job
        {
            public uint Id { get; }
            public string Name { get; }
            public List<Task> TaskList { get; private set; }

            public Job(uint id)
            {
                Id = id;
                Name = $"J_{Id}";
                TaskList = new List<Task>();
            }

            private uint GetNewTaskId()
            {
                return (uint)(TaskList.Count + 1);
            }

            public void AddTask(List<Machine> machines, List<uint> durations)
            {
                TaskList.Add(new Task(GetNewTaskId(), Id, machines, durations));
            }

            public string DebugString()
            {
                var sb = new StringBuilder($"{Name}:");
                TaskList.ForEach(t => sb.Append($"\n{t.DebugString()}"));
                return sb.ToString();
            }
        }


        /// The name of the jobshop instance.
        public string Name { get; }
        public List<Job> JobList { get; private set; }
        public List<Machine> MachineList { get; private set; }

        public FlexibleJobShopData(string name)
        {
            if (string.IsNullOrWhiteSpace(name))
            {
                Name = "unnamed";
            }    
            else
            {
                Name = name;
            }
            JobList = new List<Job>();
            MachineList = new List<Machine>();
        }

        public uint JobCount()
        {
            return (uint)JobList.Count;
        }

        public uint MachineCount()
        {
            return (uint)MachineList.Count;
        }

        private uint GetNewJobId()
        {
            return JobCount() + 1U;
        }

        private uint GetNewMachineId()
        {
            return MachineCount() + 1U;
        }

        public Job MakeJob()
        {
            var id = GetNewJobId();
            var j = new Job(id);
            JobList.Add(j);
            return j;
        }

        public Machine MakeMachine()
        {
            var id = GetNewMachineId();
            var m = new Machine(id);
            MachineList.Add(m);
            return m;
        }

        public string DebugString()
        {
            var sb = new StringBuilder($"{Name}: {JobCount()} jobs, {MachineCount()} machines");
            sb.Append("\nMachines:");
            MachineList.ForEach(m => sb.Append($" {m.Name}"));
            sb.Append("\nJobs:");
            JobList.ForEach(j => sb.Append($"\n{j.DebugString()}"));
            return sb.ToString();
        }

    }


    public class FlexibleJobShop
    {
        private class TaskAlternative : IntervalVarVector
        {
            public uint JobId { get; }
            public IntVar AlternativeVar { get; set; }

            public TaskAlternative(uint jobId, IntVar alternativeVar = null)
            {
                JobId = jobId;
                AlternativeVar = alternativeVar;
            }
        }


        public static void Solve(FlexibleJobShopData data, bool chatty = false)
        {
            // Compute horizon
            var horizon = data.JobList.Sum(
                j => j.TaskList.Sum(
                    t => t.Durations.Max()));

            
            // ----- Create all intervals and vars -----

            /*
            // Use some profiling and change the default parameters of the solver
            SolverParameters solverParams = new SolverParameters();
            // Change the profile level
            solverParams.profile_level = SolverParameters.NORMAL_PROFILING;
            // Constraint programming engine
            Solver solver = new Solver("JobShop", solverParams);
            */
            // Constraint programming engine
            Solver solver = new Solver($"FlexibleJobShop: {data.Name}");

            // Initialize dictionaries to hold references to task intervals
            var tasksByJobId = new Dictionary<uint, List<TaskAlternative>>();
            foreach (var job in data.JobList)
            {
                tasksByJobId[job.Id] = new List<TaskAlternative>(job.TaskList.Count);
            }
            var tasksByMachineId = new Dictionary<uint, IntervalVarVector>();
            foreach (var machine in data.MachineList)
            {
                tasksByMachineId[machine.Id] = new IntervalVarVector();
            }

            // Creates all individual interval variables and collect in dictionaries
            foreach (var job in data.JobList)
            {
                foreach (var task in job.TaskList)
                {
                    var alternative = new TaskAlternative(job.Id);
                    tasksByJobId[job.Id].Add(alternative);
                    var activeVariables = new IntVarVector();
                    var hasAlternatives = task.AlternativesCount > 1;
                    for (int alt = 0; alt < task.AlternativesCount; alt++)
                    {
                        var machine = task.Machines[alt];
                        var duration = task.Durations[alt];
                        var name = $"{task.Name}; Alternative {alt}: {machine.Name}, Duration {duration}";
                        IntervalVar interval = solver.MakeFixedDurationIntervalVar(0, horizon, duration, hasAlternatives, name);
                        alternative.Add(interval);
                        tasksByMachineId[machine.Id].Add(interval);
                        if (hasAlternatives)
                        {
                            activeVariables.Add(interval.PerformedExpr().Var());
                        }
                    }
                    alternative.AlternativeVar = solver.MakeIntVar(0, task.AlternativesCount - 1, task.Name);
                    if (hasAlternatives)
                    {
                        solver.Add(solver.MakeMapDomain(alternative.AlternativeVar, activeVariables));
                    }
                }
            }


            // ----- Create model -----

            // Create precedences inside jobs
            foreach (var taskAltList in tasksByJobId.Values)
            {
                for (int i = 0; i < taskAltList.Count - 1; i++)
                {
                    TaskAlternative currentTaskAlt = taskAltList[i];
                    TaskAlternative nextTaskAlt = taskAltList[i + 1];
                    foreach (var alt1 in currentTaskAlt)
                    {
                        foreach (var alt2 in nextTaskAlt)
                        {
                            solver.Add(solver.MakeIntervalVarRelation(
                                alt2, Solver.STARTS_AFTER_END, alt1));
                        }
                    }
                }
            }

            // Collect alternative variables.
            IntVarVector alternativeVariableVec = new IntVarVector();
            foreach (var taskAltList in tasksByJobId.Values)
            {
                foreach (var taskAlt in taskAltList)
                {
                    if (!taskAlt.AlternativeVar.Bound())
                    {
                        alternativeVariableVec.Add(taskAlt.AlternativeVar);
                    }
                }
            }

            // Add disjunctive constraints on unary resources, and create 
            // sequence variables. A sequence variable is a dedicated variable
            // whose job is to sequence interval variables.
            SequenceVarVector allSequences = new SequenceVarVector();
            foreach (var machine in data.MachineList)
            {
                DisjunctiveConstraint disjCt = solver.MakeDisjunctiveConstraint(
                    tasksByMachineId[machine.Id], machine.Name);
                solver.Add(disjCt);
                allSequences.Add(disjCt.SequenceVar());
            }

            // Create array of end_times of jobs
            IntVarVector endsVec = new IntVarVector();
            foreach (var taskAltList in tasksByJobId.Values)
            {
                TaskAlternative lastTaskAlt = taskAltList.Last();
                foreach (var alt in lastTaskAlt)
                {
                    endsVec.Add(alt.SafeEndExpr(-1).Var());
                }
            }

            // Objective: minimize the makespan (maximum end times of all tasks)
            // of the problem.
            IntVar objectiveVar = solver.MakeMax(endsVec).Var();
            OptimizeVar objectiveMonitor = solver.MakeMinimize(objectiveVar, 1);


            // ----- Search monitors and decision builder -----

            // This decision builder will assign all alternative variables.
            DecisionBuilder alternativePhase = solver.MakePhase(alternativeVariableVec, Solver.CHOOSE_MIN_SIZE, Solver.ASSIGN_MIN_VALUE);

            // This decision builder will rank all tasks on all machines.
            DecisionBuilder sequencePhase = solver.MakePhase(allSequences, Solver.SEQUENCE_DEFAULT);

            // After the ranking of tasks, the schedule is still loose and any
            // task can be postponed at will. But, because the problem is now a PERT
            // (http://en.wikipedia.org/wiki/Program_Evaluation_and_Review_Technique),
            // we can schedule each task at its earliest start time. This is
            // conveniently done by fixing the objective variable to its
            // minimum value.
            DecisionBuilder objectivePhase = solver.MakePhase(objectiveVar, Solver.CHOOSE_FIRST_UNBOUND, Solver.ASSIGN_MIN_VALUE);

            // The main decision builder (ranks all tasks, then fixes the
            // objective_variable).
            DecisionBuilder mainPhase = solver.Compose(alternativePhase, sequencePhase, objectivePhase);

            // Search log
            const int kLogFrequency = 1000000;
            SearchMonitor searchLog = solver.MakeSearchLog(kLogFrequency, objectiveMonitor);

            const long FLAGS_time_limit_in_ms = 1000 * 60 * 20;
            SearchLimit limit = null;
            if (FLAGS_time_limit_in_ms > 0)
            {
                limit = solver.MakeTimeLimit(FLAGS_time_limit_in_ms);
            }

            SolutionCollector collector = solver.MakeLastSolutionCollector();
            collector.AddObjective(objectiveVar);
            collector.Add(alternativeVariableVec);
            collector.Add(allSequences);

            foreach (var taskVec in tasksByMachineId.Values)
            {
                foreach (var task in taskVec)
                {
                    collector.Add(task.StartExpr().Var());
                }
            }


            // ----- Search -----

            bool solutionFound = solver.Solve(mainPhase, searchLog, objectiveMonitor, limit, collector);

            if (solutionFound)
            {
                // The index of the solution from the collector
                const int SOLUTION_INDEX = 0;
                Assignment solution = collector.Solution(SOLUTION_INDEX);

                Console.WriteLine();
                uint machineIdx = 0;
                foreach (var seq in allSequences)
                {
                    machineIdx++;
                    var taskSeq = collector.ForwardSequence(SOLUTION_INDEX, seq);
                    Console.WriteLine($"{seq.Name()}:");
                    Console.WriteLine("  Tasks: " + string.Join(", ", taskSeq));
                    
                    //foreach (var taskIndex in storedSequence)
                    //{
                    //    IntervalVar task = sequence.Interval(taskIndex);
                    //    long startMin = solution.StartMin(task);
                    //    long startMax = solution.StartMax(task);
                    //    if (startMin == startMax)
                    //    {
                    //        Console.WriteLine($"Task {task.Name()} starts at {startMin}.");
                    //    }
                    //    else
                    //    {
                    //        Console.WriteLine($"Task {task.Name()} starts between {startMin} and {startMax}.");
                    //    }
                    //}


                    Console.WriteLine();
                    Console.Write("  Starting times:");
                    foreach (var s in taskSeq)
                    {
                        Console.Write(" " + collector.Value(0, tasksByMachineId[machineIdx][s].StartExpr().Var()).ToString());
                    }
                    Console.WriteLine();


                }
                //var x = tasksByMachineId[1][0].StartExpr().Var();
                //var xValStr = collector.Value(0, x).ToString();
                Console.WriteLine("objective function value = " + solution.ObjectiveValue());  //TEMP
            }

            // Save profile in file
            //solver.ExportProfilingOverview("profile.txt");

            // Done
            solver.EndSearch();
            
        }

        public static void Test()
        {
            var data = new FlexibleJobShopData("Test");
            var j1 = data.MakeJob();
            var j2 = data.MakeJob();
            var m1 = data.MakeMachine();
            var m2 = data.MakeMachine();
            var m3 = data.MakeMachine();
            j1.AddTask(
                new List<Machine>() { m1, m2 },
                new List<uint>() { 3, 6 });
            j1.AddTask(
                new List<Machine>() { m1, m2 },
                new List<uint>() { 3, 3 });
            j2.AddTask(
                new List<Machine>() { m1, m2 },
                new List<uint>() { 3, 6 });
            j2.AddTask(
                new List<Machine>() { m2, m3 },
                new List<uint>() { 3, 2 });
            j2.AddTask(
                new List<Machine>() { m3 },
                new List<uint>() { 3 });

            Solve(data, true);
        }
    }
}
