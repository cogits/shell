const std = @import("std");

const targets: []const std.Target.Query = &.{
    .{ .cpu_arch = .x86, .os_tag = .linux },
    .{ .cpu_arch = .x86_64, .os_tag = .linux },
    .{ .cpu_arch = .aarch64, .os_tag = .linux },
    .{ .cpu_arch = .riscv64, .os_tag = .linux },
    .{ .cpu_arch = .powerpc64le, .os_tag = .linux },

    .{ .cpu_arch = .x86_64, .os_tag = .macos },
    .{ .cpu_arch = .aarch64, .os_tag = .macos },
};

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const ast_module = b.addModule("shell", .{
        .root_source_file = b.path("shell/Ast.zig"),
    });

    const shell = b.addExecutable(.{
        .name = "shell",
        .root_source_file = b.path("main.zig"),
        .target = target,
        .optimize = optimize,
    });
    shell.root_module.addImport("Ast", ast_module);

    b.installArtifact(shell);

    const run_cmd = b.addRunArtifact(shell);
    run_cmd.step.dependOn(b.getInstallStep());

    // This allows the user to pass arguments to the application in the build
    // command itself, like this: `zig build run -- arg1 arg2 etc`
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    // Creates a step for unit testing. This only builds the test executable
    // but does not run it.
    const lib_unit_tests = b.addTest(.{
        .root_source_file = b.path("shell/Ast.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);

    // Similar to creating the run step earlier, this exposes a `test` step to
    // the `zig build --help` menu, providing a way for the user to request
    // running the unit tests.
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_unit_tests.step);

    // release
    const release_step = b.step("release", "Build for multiple targets to make a release");
    for (targets) |t| {
        const exe = b.addExecutable(.{
            .name = b.fmt("shell-{s}-{s}", .{ @tagName(t.cpu_arch.?), @tagName(t.os_tag.?) }),
            .root_source_file = b.path("main.zig"),
            .target = b.resolveTargetQuery(t),
            .optimize = .ReleaseSmall,
        });
        exe.root_module.addImport("Ast", ast_module);

        const target_output = b.addInstallArtifact(exe, .{
            .dest_dir = .{ .override = .{ .custom = "release" } },
        });

        release_step.dependOn(&target_output.step);
    }
}
